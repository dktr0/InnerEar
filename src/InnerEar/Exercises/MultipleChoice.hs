{-# LANGUAGE RecursiveDo #-}

module InnerEar.Exercises.MultipleChoice where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common
import Data.Map
import Control.Monad (zipWithM)
import Data.List (findIndices,partition,elemIndex)
import Data.Maybe (fromJust)
import System.Random

import InnerEar.Types.ExerciseId
import InnerEar.Types.Data
import InnerEar.Types.Exercise
import InnerEar.Types.ExerciseNavigation
import InnerEar.Types.Score
import InnerEar.Types.Utility
import InnerEar.Widgets.Utility
import InnerEar.Widgets.UserMedia
import InnerEar.Widgets.AnswerButton
import Reflex.Synth.Types

-- | This module introduces a function to generate multiple choice exercises.
-- Most specifically, it abstracts over the requirement to provide a widget
-- to present questions, requiring instead that a fixed list of possible
-- answers be provided, together with a pure function that converts an answer
-- value to a sound.

multipleChoiceExercise :: (MonadWidget t m, Show a, Eq a, Ord a)
  => Int -- maximum number of tries to allow
  -> [a]
  -> m ()
  -> (c->m (Dynamic t c,  Dynamic t Source,  Event t (Maybe a))) -- dyn config, source, and event maybe answer for playing reference sound (config widget)
  -> (c -> Source -> Maybe a -> Sound) -- function to produce a sound from an answer, where a Nothing answer is to be interpreted as a reference sound (or
  -> ExerciseId
  -> c
  -> (c -> m (Event t c))
  -> (Dynamic t (Map a Score) -> m ())
  -> (c -> [Datum c [a] a (Map a Score)] -> IO ([a],a))
  -> Exercise t m c [a] a (Map a Score)

multipleChoiceExercise maxTries answers iWidget cWidget render i c cw de g = Exercise {
  exerciseId = i,
  instructionsWidget = iWidget,
  defaultConfig = c,
  configWidget = cw,
  defaultEvaluation = empty,
  displayEvaluation = de,
  generateQuestion = g,
  questionWidget = multipleChoiceQuestionWidget maxTries answers i iWidget cWidget render de
  }

multipleChoiceQuestionWidget :: (MonadWidget t m, Show a, Eq a, Ord a)
  => Int -- maximum number of tries
  -> [a] -- fixed list of potential answers
  -> ExerciseId
  -> m ()
  -> (c->m (Dynamic t c,  Dynamic t Source,  Event t (Maybe a))) -- dyn config, source, and event maybe answer for playing reference sound (config widget)
  -> (c -> Source -> Maybe a -> Sound) -- function to produce a sound from an answer, where a Nothing answer is to be interpreted as a reference sound (or some other sound not a question)
  -> (Dynamic t (Map a Score) -> m ())
  -> c
  -> Map a Score
  -> Event t ([a],a)
  -> m (Event t (Datum c [a] a (Map a Score)),Event t Sound,Event t c,Event t ExerciseNavigation)

multipleChoiceQuestionWidget maxTries answers exId exInstructions cWidget render eWidget config initialEval newQuestion = elClass "div" "exerciseWrapper" $ mdo

  let initialState = initialMultipleChoiceState answers maxTries
  let newQuestion' = fmap newQuestionMultipleChoiceState newQuestion
  questionHeard0 <- holdDyn False $ leftmost [False <$ newQuestion,True <$ playQuestion]
  let questionHeard = nubDyn questionHeard0
  let questionHeard' = fmap (const onceQuestionHeard) $ ffilter (==True) $ updated questionHeard
  let answerPressed' = fmap answerSelected answerPressed
  let stateChanges = leftmost [newQuestion',questionHeard', answerPressed']
  multipleChoiceState <- foldDyn ($) initialState stateChanges
  modes <- mapDyn answerButtonModes multipleChoiceState
  modes' <- mapM (\x-> mapDyn (!!x) modes) [0,1..9]
  scores <- mapDyn scoreMap multipleChoiceState

  -- user interface
  (closeExercise,playQuestion,answerPressed,nextQuestionNav) <- elClass "div" "topRow" $ do
    w <- elClass "div" "topRowHeader" $ do
      elClass "div" "questionTitle" $ text $ ("Exercise: " ++ showExerciseTitle exId)
      elClass "div" "closeExerciseButton" $ buttonClass "Close" "closeExerciseButton"
    (x,y,z) <- elClass "div" "buttonInterface" $ do
      x <- elClass "div" "listenButton" $ buttonClass "Listen" "listenButton"
      y <- elClass "div" "answerButtonWrapper" $ do
        leftmost <$> zipWithM (\f m -> answerButton (show f) m f) answers modes'
      z <- elClass "div" "nextButton" $ revealableButton "Next" "nextButton" questionHeard
      return (x,y,z)
    return (CloseExercise <$ w,x,y,InQuestion <$ z)

  (dynConfig, dynSource, playReference) <- elClass "div" "middleRow" $ do
    elClass "div" "evaluation" $ exInstructions
    elClass "div" "journal" $ do
      text "Configuration"
      elClass "div"  "configWidgetWrapper" $ cWidget config

  journalData <- elClass "div" "bottomRow" $ do
    elClass "div" "evaluation" $ do
      eWidget scores
      display scores
    elClass "div" "journal" $ journalWidget

  let answerEvent = gate (fmap (==AnswerMode) . fmap mode . current $ multipleChoiceState) answerPressed
  let exploreEvent = gate (fmap (==ExploreMode) . fmap mode . current $ multipleChoiceState) answerPressed

  -- generate sounds to be played
  answer <- holdDyn Nothing $ fmap (Just . snd) newQuestion

  let questionSound = fmapMaybe id $ tagDyn answer playQuestion
  let soundsToRender = leftmost [fmap Just questionSound, fmap Just exploreEvent, playReference]
  sourceAndConfig <- combineDyn (,) dynConfig dynSource
  let playSounds = attachDynWith (\(c,s) r->render c s r) sourceAndConfig soundsToRender

  let navEvents = leftmost [closeExercise,nextQuestionNav]

  -- generate data for adaptive questions and analysis
  let questionWhileListened = (\x -> (possibleAnswers x,correctAnswer x)) <$> tagDyn multipleChoiceState playQuestion
  let listenedQuestionData = attachDynWith (\c (q,a)-> ListenedQuestion c q a) dynConfig questionWhileListened

  let questionWhileReference = (\x -> (possibleAnswers x,correctAnswer x)) <$> tagDyn multipleChoiceState playReference
  let listenedReferenceData = attachDynWith (\c (q,a) -> ListenedReference c q a) dynConfig questionWhileReference
  evaluations <- mapDyn scoreMap multipleChoiceState
  mcsAndConfig <- combineDyn (,) dynConfig multipleChoiceState
  let answerWithContext = attachDynWith (\(c,mcs) s -> (s, c, possibleAnswers mcs, correctAnswer mcs)) mcsAndConfig answerEvent
  let answerData = attachDynWith (\e (s,c,q,a) -> Answered s e e c q a) evaluations answerWithContext
  let questionWhileExplore = attachDynWith (\x y -> (possibleAnswers x,correctAnswer x,y)) multipleChoiceState answerPressed
  let listenedExploreData = attachDynWith (\c (q,a,s) -> ListenedExplore s c q a) dynConfig questionWhileExplore
  let datums = leftmost [listenedQuestionData,listenedReferenceData, answerData,listenedExploreData,journalData]

  return (datums, playSounds,updated dynConfig,navEvents)

journalWidget :: MonadWidget t m => m (Event t (Datum c q a e))
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

radioConfigWidget :: (MonadWidget t m, Eq a, Show a) => String -> String -> [a] -> a -> m (Event t a)
radioConfigWidget explanation msg possibilities i = do
  let radioButtonMap =  zip [0::Int,1..] possibilities
  let iVal = maybe 0 id $ elemIndex i possibilities
  elClass "div" "explanation" $ text explanation
  elClass "div" "configText" $ text msg
  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ fmap (\(x,y)->(x,show y)) radioButtonMap)
           (WidgetConfig {_widgetConfig_initialValue= Just iVal
                         ,_widgetConfig_setValue = never
                         ,_widgetConfig_attributes = constDyn empty})
  dynConfig <- holdDyn i $ fmap (\x-> maybe i id $ Data.Map.lookup (maybe 0 id x) (fromList radioButtonMap)) (_hwidget_change radioWidget)
  b <- button "Begin Exercise"
  return $ tagDyn dynConfig b

trivialBWidget :: MonadWidget t m => m (Dynamic t ())
trivialBWidget = holdDyn () $ never


data MultipleChoiceMode = ListenMode | AnswerMode | ExploreMode deriving (Eq)

data MultipleChoiceState a = MultipleChoiceState {
  mode :: MultipleChoiceMode,
  correctAnswer :: a,
  allAnswers :: [a],
  possibleAnswers :: [a],
  answerButtonModes :: [AnswerButtonMode],
  attemptsRemainingDefault :: Int,
  attemptsRemaining :: Int,
  scoreMap :: Map a Score
  }

-- initialMultipleChoiceState provides a useful initial configuration of
-- the MultipleChoiceState for the time before a new question has been
-- generated.

initialMultipleChoiceState :: [a] -> Int -> MultipleChoiceState a
initialMultipleChoiceState xs n = MultipleChoiceState {
  mode = ListenMode,
  correctAnswer = xs!!0,
  allAnswers = xs,
  possibleAnswers = xs,
  answerButtonModes = NotPossible <$ xs,
  attemptsRemainingDefault = n,
  attemptsRemaining = n,
  scoreMap = empty
  }

-- When a multiple choice question is generated, all of the buttons are
-- not possible, pending the user listening to the correct answer.

newQuestionMultipleChoiceState :: ([a],a) -> MultipleChoiceState a -> MultipleChoiceState a
newQuestionMultipleChoiceState (xs,x) s = s {
  mode = ListenMode,
  correctAnswer = x,
  possibleAnswers = xs,
  answerButtonModes = NotPossible <$ allAnswers s,
  attemptsRemaining = attemptsRemainingDefault s
  }

-- Once the user has listened to the correct answer at least once, all
-- of the buttons that represent possible answers become possible and the
-- mode changes to AnswerMode (the only mode in which answers are processed)

onceQuestionHeard :: Eq a => MultipleChoiceState a -> MultipleChoiceState a
onceQuestionHeard s = s { mode = AnswerMode, answerButtonModes = m }
  where m = fmap f $ fmap (flip elem $ possibleAnswers s) $ allAnswers s
        f True = Possible
        f False = NotPossible

-- When answers are selected they are ignored if mode is ListenMode or ExploreMode
-- Otherwise (i.e. AnswerMode) the state is updated in different ways depending
-- on whether the answer is correct or incorrect, and

answerSelected :: (Eq a,Ord a) => a -> MultipleChoiceState a -> MultipleChoiceState a
answerSelected _ s | mode s == ListenMode = s
answerSelected _ s | mode s == ExploreMode = s

answerSelected a s | a == correctAnswer s = toExploreMode $ s {
      answerButtonModes = replaceAtSameIndex a (allAnswers s) Correct (answerButtonModes s),
      scoreMap = markCorrect a $ scoreMap s
      }

answerSelected a s | a /= correctAnswer s && attemptsRemaining s > 1 = s {
      answerButtonModes = replaceAtSameIndex a (allAnswers s) IncorrectDisactivated (answerButtonModes s),
      attemptsRemaining = attemptsRemaining s - 1,
      scoreMap = markIncorrect a (correctAnswer s) $ scoreMap s
      }

answerSelected a s | a /= correctAnswer s && attemptsRemaining s <= 1 = toExploreMode $ s {
      answerButtonModes = replaceAtSameIndex a (allAnswers s) IncorrectActivated (answerButtonModes s),
      scoreMap = markIncorrect a (correctAnswer s) $ scoreMap s
      }

toExploreMode :: MultipleChoiceState a -> MultipleChoiceState a
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
