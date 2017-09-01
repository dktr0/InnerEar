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
import InnerEar.Widgets.UserMedia
import InnerEar.Widgets.AnswerButton
import Reflex.Synth.Types

-- | This module introduces a function to generate multiple choice exercises.
-- Most specifically, it abstracts over the requirement to provide a widget
-- to present questions, requiring instead that a fixed list of possible
-- answers be provided, together with a pure function that converts an answer
-- value to a sound.

multipleChoiceExercise :: (MonadWidget t m, Show a, Eq a, Ord a)
  => [a]
  -> m (Dynamic t b) -- b represents something which affects sound production independently of configuration
  -> (c -> b -> a -> Sound)
  -> ExerciseId
  -> c
  -> (c -> m (Event t c))
  -> (Dynamic t (Map a Score) -> m ())
  -> (c -> [Datum c [a] a (Map a Score)] -> IO ([a],a))
  -> Maybe Reflection
  -> Exercise t m c [a] a (Map a Score)

multipleChoiceExercise answers bWidget render i c cw de g r = Exercise {
  exerciseId = i,
  defaultConfig = c,
  configWidget = cw,
  defaultEvaluation = empty,
  displayEvaluation = de,
  generateQuestion = g,
  questionWidget = multipleChoiceQuestionWidget answers bWidget render,
  reflectiveQuestion = r
}

  ---> (([a],a) -> m (Dynamic t Sound))

multipleChoiceQuestionWidget :: (MonadWidget t m, Show a, Eq a, Ord a)
  => [a] -- fixed list of potential answers
  -> m (Dynamic t b) -- b represents something which affects sound production independently of configuration
  -> (c -> b -> a -> Sound) -- function to produce a sound from an answer
  -> c
  -> Map a Score
  -> Event t ([a],a)

  -> m (Event t (Datum c [a] a (Map a Score)),Event t Sound,Event t ExerciseNavigation)

multipleChoiceQuestionWidget answers bWidget render config initialEval newQuestion = mdo
  let maxTries = 3::Int

  b <- bWidget

  -- Managing number of tries
  listOfClicked <- foldDyn ($) [] $ leftmost [fmap (:) bandPressed, (const []) <$ newQuestion]
  let tryEv = attachWithMaybe (\l e -> if elem e l then Nothing else Just e) (current listOfClicked) bandPressed
  tries <- foldDyn ($) 0 $ leftmost [(+1) <$ tryEv, (const 0) <$ nextQuestion]
  canTry <- mapDyn (<maxTries) tries >>= combineDyn (&&) notCorrectYet -- Dynamic t Bool
  let cannotTryEv = ffilter not $ updated canTry -- Event t Bool (true when change to not able to try)

  -- produce events for correct and incorrect answers
  question <- holdDyn Nothing $ fmap (Just . fst) newQuestion -- m (Dynamic t (Maybe [a]))
  answer <- holdDyn Nothing $ fmap (Just . snd) newQuestion -- m (Dynamic t (Maybe a))
  let answerEvent = gate (current canTry) tryEv -- Event t (Maybe a)
  notCorrectYet <- holdDyn True $ leftmost [True <$ newQuestion, False <$ correctAnswer]
  let correctAnswer = attachDynWithMaybe (\x y -> if (fromJust x)==y then x else Nothing) answer answerEvent  -- Event t a
  let incorrectAnswer = attachDynWithMaybe (\x y -> if (fromJust x)/=y then x else Nothing) answer answerEvent -- Event t a
  let incorrectNotFinal = attachDynWithMaybe (\x y -> if x then Just y else Nothing) canTry incorrectAnswer --Event t a
  let incorrectFinal = attachDynWithMaybe (\x y -> if (not x) then Just y else Nothing) canTry incorrectAnswer -- Event t a

  -- use new questions, correct and incorrect answer events to calculate button modes
  let initialModes = fmap (const NotPossible) answers -- [AnswerButtonMode]
  let newModes = fmap (modesForNewQuestion answers) newQuestion
  let attemptModes = fmap (modesForIncorrectAnswer answers) incorrectNotFinal
  let incorrectModes = fmap (modesForExplore .)  $ fmap (modesForIncorrectAnswer answers) incorrectFinal
  let correctModes = fmap (modesForExplore .) $ fmap (modesForCorrectAnswer answers) correctAnswer
  modes <- foldDyn ($) initialModes $ leftmost [newModes,attemptModes,incorrectModes,correctModes]
  modes' <- mapM (\x-> mapDyn (!!x) modes) [0,1..9]

  -- buttons
  playUnfiltered <- button "Listen to unfiltered"
  playButton <- button "Play question"
  bandPressed <- elClass "div" "answerButtonWrapper" $ -- m (Event t a)
    leftmost <$> zipWithM (\f m -> answerButton (show f) m f) answers modes'

  -- update scoreMap
  let correctAnswerScoreUpdate = attachDynWith (\x y -> (fromJust x,Right y)) answer correctAnswer
  let incorrectAnswerScoreUpdate = attachDynWith (\x y -> (fromJust x,Left y)) answer incorrectAnswer
  let answerInfo = leftmost [correctAnswerScoreUpdate,incorrectAnswerScoreUpdate]
  let scoreUpdate = attachWith updateScore (current scoreMap) answerInfo -- Event t (Map a Score)
  scoreMap <- holdDyn initialEval scoreUpdate -- Dynamic t (Map a Score)

  -- display feedback
  let resetFeedback = fmap (const "") navEvents
  let correctAnswerFeedback = "Correct!" <$ correctAnswer
  let incorrectAnswerFeedback = "Incorrect" <$ incorrectAnswer
  feedbackToDisplay <- holdDyn "" $ leftmost [correctAnswerFeedback,incorrectAnswerFeedback]
  dynText feedbackToDisplay

  -- generate sounds to be played

  let playCorrectSound = fromJust <$> tagDyn answer playButton
  let unfilteredSound = Sound (NodeSource (BufferNode $ File "pinknoise.wav") 2.0) <$ playUnfiltered
  let answersToRender = leftmost [playCorrectSound,bandPressed]
  let renderedAnswers = attachDynWith (render config) b answersToRender
  let playSounds = leftmost [renderedAnswers,unfilteredSound]

  -- generate navigation events
  nextQuestion <- (InQuestion <$) <$> button "New Question"
  onToReflect <- (InReflect <$) <$> button "Reflect"
  let navEvents = leftmost [nextQuestion,onToReflect]

  el "div" $ do
    text "debugging:   "
    el "div"$ do
      text "tries:  "
      mapDyn show tries >>= dynText
    el "div"$ do
      text "correct answer:  "
      mapDyn show answer >>= dynText
    el "div" $ do
      text "canAnswer: "
      mapDyn show canTry >>= dynText
    el "div"$ do
      text "userAnswer:  "
      holdDyn "nothing" (fmap show bandPressed) >>= dynText
    el "div"$ do
      text "Score Map:  "
      mapDyn show scoreMap >>= dynText

  return (fmap Evaluation (updated scoreMap), playSounds,navEvents)


modesForNewQuestion :: (Eq a) => [a] -> ([a],a) -> [AnswerButtonMode] -> [AnswerButtonMode]
modesForNewQuestion possibleAnswers question _ = fmap f $ fmap (flip elem $ fst question) possibleAnswers
  where f True = Possible
        f False = NotPossible

modesForCorrectAnswer :: (Eq a) => [a] -> a -> [AnswerButtonMode] -> [AnswerButtonMode]
modesForCorrectAnswer possibleAnswers answer xs = replaceAtSameIndex answer possibleAnswers Correct xs

modesForIncorrectAnswer :: (Eq a) => [a] -> a -> [AnswerButtonMode] -> [AnswerButtonMode]
modesForIncorrectAnswer possibleAnswers answer xs = replaceAtSameIndex answer possibleAnswers IncorrectDisactivated xs

modesForExplore :: [AnswerButtonMode] -> [AnswerButtonMode]
modesForExplore = fmap f
  where
    f NotPossible = NotPossible
    f Possible = Possible
    f IncorrectDisactivated = IncorrectActivated
    f IncorrectActivated = IncorrectActivated
    f Correct = Correct

randomMultipleChoiceQuestion :: [a] -> IO ([a],a)
randomMultipleChoiceQuestion possibilities = do
  let n = length possibilities
  x <- getStdRandom ((randomR (0,n-1))::StdGen -> (Int,StdGen))
  return (possibilities,possibilities!!x)

radioConfigWidget :: (MonadWidget t m, Eq a, Show a) => String -> [a] -> a -> m (Event t a)
radioConfigWidget msg possibilities i = do
  let radioButtonMap =  zip [0::Int,1..] possibilities
  let iVal = maybe 0 id $ elemIndex i possibilities
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
