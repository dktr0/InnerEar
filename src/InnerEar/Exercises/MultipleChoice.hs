{-# LANGUAGE DeriveDataTypeable, RecursiveDo, ScopedTypeVariables #-}

module InnerEar.Exercises.MultipleChoice where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Common
import Data.Map
import Control.Monad (zipWithM,liftM)
import Control.Monad.IO.Class (liftIO)
import Data.List (findIndices,partition,elemIndex)
import Data.Maybe (fromJust)
import System.Random
import Text.JSON
import Text.JSON.Generic

import InnerEar.Types.ExerciseId
import InnerEar.Types.Data hiding (Time)
import InnerEar.Types.Exercise
import InnerEar.Types.ExerciseNavigation
import InnerEar.Types.Score
import InnerEar.Types.MultipleChoiceStore
import InnerEar.Types.Utility
import InnerEar.Widgets.Utility
import InnerEar.Widgets.AnswerButton
import Sound.MusicW

-- | This module introduces a function to generate multiple choice exercises.
-- Most specifically, it abstracts over the requirement to provide a widget
-- to present questions, requiring instead that a fixed list of possible
-- answers be provided, together with a pure function that converts an answer
-- value to a sound.

-- | AnswerRenderer takes the system resources, a question configuration, sound source,
-- and a potential answer and builds a `Synth` for that answer. If the answer is `Nothing` a
-- reference synth should be constructed.
type AnswerRenderer c a = Map String AudioBuffer -> c -> (SourceNodeSpec, Maybe Time) -> Maybe a -> Synth ()

-- | ConfigWidgetBuilder constructs a configuration widget with a given default configuration.
type ConfigWidgetBuilder m t c a = Dynamic t (Map String AudioBuffer) -> c -> m (Dynamic t c, Dynamic t (Maybe (SourceNodeSpec, Maybe Time)), Event t (), Event t ())

multipleChoiceExercise :: forall t m c a. (MonadWidget t m, Show a, Eq a, Ord a, Data a, Data c, Ord c, Show c, Buttonable a)
  => Int -- maximum number of tries to allow
  -> [a]
  -> m ()
  -> ConfigWidgetBuilder m t c a
  -> AnswerRenderer c a
  -> ExerciseId
  -> c
  -> (Dynamic t (Map a Score) -> Dynamic t (MultipleChoiceStore c a) -> m ())
  -> (c -> [ExerciseDatum] -> IO ([a],a))
  -> XpFunction c a
  -> Exercise t m c [a] a (Map a Score) (MultipleChoiceStore c a)

multipleChoiceExercise maxTries answers iWidget cWidget render i c de g calculateXp = Exercise {
  exerciseId = i,
  instructionsWidget = iWidget,
  defaultConfig = c,
  defaultStore = newStoreWithNoScores calculateXp,
  defaultEvaluation = empty,
  displayEvaluation = de,
  generateQuestion = g,
  questionWidget = multipleChoiceQuestionWidget maxTries answers i iWidget cWidget render de calculateXp
  }

multipleChoiceQuestionWidget :: forall t m c a. (MonadWidget t m, Show a, Eq a, Ord a,Data a,Data c,Ord c, Show c, Buttonable a)
  => Int -- maximum number of tries
  -> [a] -- fixed list of potential answers
  -> ExerciseId
  -> m ()
  -> ConfigWidgetBuilder m t c a
  -> AnswerRenderer c a
  -> (Dynamic t (Map a Score) -> Dynamic t (MultipleChoiceStore c a) -> m ())
  -> XpFunction c a
  -> MultipleChoiceStore c a
  -> Dynamic t (Map String AudioBuffer)
  -> c
  -> Map a Score
  -> Event t ([a],a)
  -> m (Event t (MultipleChoiceStore c a),Event t ExerciseDatum,Event t (Maybe (Synth ())),Event t c,Event t ExerciseNavigation)

multipleChoiceQuestionWidget maxTries answers exId exInstructions cWidget render eWidget xpF initialStore sysResources config initialEval newQuestion = elClass "div" "exerciseWrapper" $ mdo

  -- calculation of current value of exercise' MultipleChoiceStore
  let scoreChanges = traceEventWith (const "scoreChanges") $ attachWith answerToScoreChange (current multipleChoiceState) answerPressed -- Event t (Maybe (c,Map a Score -> Map a Score))
  let scoreChanges' = traceEventWith (const "scoreChanges'") $ fmapMaybe id scoreChanges -- Event t (c,Map a Score -> Map a Score)
  let scoreChanges'' = traceEventWith (const "scoreChanges''") $ fmap (newScores xpF) scoreChanges' -- Event t (MultipleChoiceStore -> MultipleChoiceStore)
  let toRecentAnswers = attachWithMaybe answerToRecentAnswer (current multipleChoiceState) answerPressed
  let toRecentAnswers' = fmap (newAnswer xpF) toRecentAnswers
  let currentStoreChanges = mergeWith (.) [scoreChanges'',toRecentAnswers']
  currentStore <- foldDyn ($) initialStore $ currentStoreChanges
  let storeEvents = updated currentStore

  -- display currentStore -- for debugging


  let initialState = initialMultipleChoiceState config answers maxTries
  let newQuestionAndConfig = attachDyn dynConfig newQuestion
  let newQuestion' = fmap (\(c, q) -> newQuestionMultipleChoiceState c q) newQuestionAndConfig
  questionHeard0 <- holdDyn False $ leftmost [False <$ newQuestion, True <$ listenPressed]
  let questionHeard = nubDyn questionHeard0
  let questionHeard' = fmap (const onceQuestionHeard) $ ffilter (== True) $ updated questionHeard
  let answerPressed' = fmap answerSelected answerPressed
  let stateChanges = leftmost [newQuestion', questionHeard', answerPressed']



  multipleChoiceState <- foldDyn ($) initialState stateChanges
  modes <- mapDyn answerButtonModes multipleChoiceState
  modes' <- mapM (\x-> mapDyn (!!x) modes) [0,1..9]

  -- MC question controls and answer input.
  -- (Event t ExerciseNavigation, Event t (), Event t a, Event t ExerciseNavigation)
  (closeExercise, listenPressed, answerPressed, nextQuestionNav) <- elClass "div" "topRow" $ do
    w <- elClass "div" "topRowHeader" $ do
      elClass "div" "questionTitle" $ text $ ("Exercise: " ++ showExerciseTitle exId)
      elClass "div" "closeExerciseButton" $ buttonClass "Close" "closeExerciseButton"
    (x,y,z) <- elClass "div" "buttonInterface" $ do
      x <- elClass "div" "listenButton" $ buttonClass "Listen" "listenButton"
      y <- elClass "div" "answerButtonWrapper" $ do
        -- leftmost <$> zipWithM (\f m -> answerButton (show f) m f) answers modes'
        leftmost <$> zipWithM makeButton answers modes'

      z <- elClass "div" "nextButton" $ revealableButton "Next" "nextButton" questionHeard
      return (x, y, z)
    return (CloseExercise <$ w, x, y, InQuestion <$ z)

  -- Instructions and configuration widgets.
  (dynConfig, dynSource, playPressed, stopPressed, journalData) <- elClass "div" "middleRow" $ do
    (a,b,c,d) <- elClass "div" "leftBox" $ do
      text "Configuration"
      elClass "div"  "configWidgetWrapper" $ cWidget sysResources config
    j <- elClass "div" "rightBox" $ journalWidget
    return (a,b,c,d,j)

  elClass "div" "bottomRow" $ do
    elClass "div" "leftBox" $ do
      scoresForCurrentConfig <- combineDyn (\c s -> findWithDefault empty c $ scores s)   dynConfig currentStore
      elClass "div" "evaluation" $ eWidget scoresForCurrentConfig currentStore
    elClass "div" "rightBox" $ exInstructions

  let answerEvent = gate (fmap (==AnswerMode) . fmap mode . current $ multipleChoiceState) answerPressed
  let correctAnswerEvent = attachDynWithMaybe (\x y -> if y == correctAnswer x then Just y else Nothing) multipleChoiceState answerEvent
  let incorrectAnswerEvent = attachDynWithMaybe (\x y -> if y /= correctAnswer x then Just y else Nothing) multipleChoiceState answerEvent
  let exploreAnswerPressed = gate (fmap (==ExploreMode) . fmap mode . current $ multipleChoiceState) answerPressed

  -- generate sounds to be played
  answer <- holdDyn Nothing $ fmap (Just . snd) newQuestion

  let listenToQuestionPressed = fmapMaybe id $ tagDyn answer listenPressed

  playbackSynthChanged <- connectPlaybackControls
    listenToQuestionPressed exploreAnswerPressed playPressed stopPressed
    dynConfig dynSource sysResources render

  let navEvents = leftmost [closeExercise, nextQuestionNav]

  -- generate data for adaptive questions and analysis
  let listenedQuestionData = ListenedQuestion <$ listenPressed
  let listenedReferenceData = ListenedReference <$ playPressed
  let incorrectAnswerData = fmap (\(s,a) -> IncorrectAnswer a s) $ attachDyn currentStore incorrectAnswerEvent
  let correctAnswerData = fmap CorrectAnswer $ tagDyn currentStore correctAnswerEvent
  let listenedExploreData = fmap ListenedExplore exploreAnswerPressed
  let datums = leftmost [listenedQuestionData,listenedReferenceData,incorrectAnswerData,correctAnswerData,listenedExploreData,journalData] :: Event t (Datum c [a] a (Map a Score) (MultipleChoiceStore c a))
  let datums' = fmap toExerciseDatum datums
  return (storeEvents,datums', playbackSynthChanged, updated dynConfig, navEvents)

connectPlaybackControls :: MonadWidget t m
  => Event t a
  -> Event t a
  -> Event t ()
  -> Event t ()
  -> Dynamic t c
  -> Dynamic t (Maybe (SourceNodeSpec, Maybe Time))
  -> Dynamic t (Map String AudioBuffer)
  -> (Map String AudioBuffer -> c -> (SourceNodeSpec, Maybe Time) -> Maybe a -> Synth ())
  -> m (Event t (Maybe (Synth ())))
connectPlaybackControls playQuestion exploreAnswer playReference stop dynConfig dynSrc sysResources render = do
  let triggerPlay = leftmost [Just <$> playQuestion, Just <$> exploreAnswer, Nothing <$ playReference]
  let triggerStop = Nothing <$ stop
  render' <- mapDyn render sysResources -- Dynamic t (c -> (SourceNodeSpec, Maybe Time) -> Maybe a -> Synth ())
  render'' <- combineDyn ($) render' dynConfig -- Dynamic t ((SourceNodeSpec,Maybe Time) -> Maybe a -> Synth ())
  render''' <- combineDyn (\x y -> maybe (const Nothing) (\z -> Just . x z) y) render'' dynSrc
  let triggerPlay' = attachDynWith ($) render''' triggerPlay
  return $ leftmost [triggerStop, triggerPlay']

journalWidget :: MonadWidget t m => m (Event t (Datum c q a e s))
journalWidget = elClass "div" "journalItem" $ mdo
  let attrs = constDyn $ fromList $ zip ["class"] ["journalItem"]
  let resetText = "" <$ b
  text "Journal"
  t <- textArea $ def & textAreaConfig_attributes .~ attrs & textAreaConfig_setValue .~ resetText
  b <- button "Save"
  return $ Reflection <$> tag (current $ _textArea_value t) b

randomMultipleChoiceQuestion :: [a] -> IO ([a],a)
randomMultipleChoiceQuestion possibilities = do
  let n = length possibilities
  x <- getStdRandom ((randomR (0,n-1))::StdGen -> (Int,StdGen))
  return (possibilities,possibilities!!x)

trivialBWidget :: MonadWidget t m => m (Dynamic t ())
trivialBWidget = holdDyn () $ never


data MultipleChoiceMode = ListenMode | AnswerMode | ExploreMode deriving (Eq)

data MultipleChoiceState a c = MultipleChoiceState {
  mode :: MultipleChoiceMode,
  currentConfig :: c,
  correctAnswer :: a,
  allAnswers :: [a],
  possibleAnswers :: [a],
  answerButtonModes :: [AnswerButtonMode],
  attemptsRemainingDefault :: Int,
  attemptsRemaining :: Int
  }


-- initialMultipleChoiceState provides a useful initial configuration of
-- the MultipleChoiceState for the time before a new question has been
-- generated.

initialMultipleChoiceState :: Ord c => c -> [a] -> Int -> MultipleChoiceState a c
initialMultipleChoiceState c xs n = MultipleChoiceState {
  mode = ListenMode,
  currentConfig = c,
  correctAnswer = xs!!0,
  allAnswers = xs,
  possibleAnswers = xs,
  answerButtonModes = NotPossible <$ xs,
  attemptsRemainingDefault = n,
  attemptsRemaining = n
  }

-- When a multiple choice question is generated, all of the buttons are
-- not possible, pending the user listening to the correct answer.

newQuestionMultipleChoiceState :: Ord c => c -> ([a],a) -> MultipleChoiceState a c -> MultipleChoiceState a c
newQuestionMultipleChoiceState c (xs,x) s = s {
  mode = ListenMode,
  currentConfig = c,
  correctAnswer = x,
  possibleAnswers = xs,
  answerButtonModes = NotPossible <$ allAnswers s,
  attemptsRemaining = attemptsRemainingDefault s
  }

-- Once the user has listened to the correct answer at least once, all
-- of the buttons that represent possible answers become possible and the
-- mode changes to AnswerMode (the only mode in which answers are processed)

onceQuestionHeard :: Eq a => MultipleChoiceState a c -> MultipleChoiceState a c
onceQuestionHeard s = s { mode = AnswerMode, answerButtonModes = m }
  where m = fmap f $ fmap (flip elem $ possibleAnswers s) $ allAnswers s
        f True = Possible
        f False = NotPossible

-- When answers are selected they are ignored if mode is ListenMode or ExploreMode
-- Otherwise (i.e. AnswerMode) the state is updated in different ways depending
-- on whether the answer is correct or incorrect, and

answerToRecentAnswer :: Eq a => MultipleChoiceState a c -> a -> Maybe (c,Bool)
answerToRecentAnswer s _ | mode s == ListenMode = Nothing
answerToRecentAnswer s _ | mode s == ExploreMode = Nothing
answerToRecentAnswer s a | a == correctAnswer s = Just (currentConfig s, True)
answerToRecentAnswer s a | a /= correctAnswer s = Just (currentConfig s, False)

answerToScoreChange :: Ord a => MultipleChoiceState a c -> a -> Maybe (c,Map a Score -> Map a Score)
answerToScoreChange s _ | mode s == ListenMode = Nothing
answerToScoreChange s _ | mode s == ExploreMode = Nothing
answerToScoreChange s a | a == correctAnswer s = Just (currentConfig s, markCorrect a)
answerToScoreChange s a | a /= correctAnswer s = Just (currentConfig s, markIncorrect a (correctAnswer s))

answerSelected :: (Eq a,Ord a,Ord c) => a -> MultipleChoiceState a c -> MultipleChoiceState a c
answerSelected _ s | mode s == ListenMode = s
answerSelected _ s | mode s == ExploreMode = s

answerSelected a s | a == correctAnswer s = toExploreMode $ s {
      answerButtonModes = replaceAtSameIndex a (allAnswers s) Correct (answerButtonModes s)
      }

answerSelected a s | a /= correctAnswer s && attemptsRemaining s > 1 = s {
      answerButtonModes = replaceAtSameIndex a (allAnswers s) IncorrectDisactivated (answerButtonModes s),
      attemptsRemaining = attemptsRemaining s - 1
      }

answerSelected a s | a /= correctAnswer s && attemptsRemaining s <= 1 = toExploreMode $ s {
      answerButtonModes = replaceAtSameIndex a (allAnswers s) IncorrectActivated $
              replaceAtSameIndex (correctAnswer s) (allAnswers s) CorrectMissed (answerButtonModes s)
      }

toExploreMode :: MultipleChoiceState a c -> MultipleChoiceState a c
toExploreMode s = s {
  mode = ExploreMode,
  answerButtonModes = fmap f $ answerButtonModes s
  }
  where
    f NotPossible = NotPossible
    f Possible = Possible
    f IncorrectDisactivated = IncorrectActivated
    f IncorrectActivated = IncorrectActivated
    f Correct = Correct
    f CorrectMissed = CorrectMissed
