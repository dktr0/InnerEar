module InnerEar.Exercises.MultipleChoice (multipleChoiceExercise) where

import Reflex
import Reflex.Dom
import Data.Map

import InnerEar.Types.ExerciseId
import InnerEar.Types.Datum
import InnerEar.Types.Exercise

-- | This module introduces a function to generate multiple choice exercises.
-- Most specifically, it abstracts over the requirement to provide a widget
-- to present questions, requiring instead that a fixed list of possible
-- answers be provided, together with a pure function that converts an answer
-- value to a sound.

multipleChoiceExercise :: (MonadWidget t m, Show a)
  => [a]
  -> (Maybe a -> Sound)
  -> ExerciseId
  -> c
  -> (c -> m (Event t c))
  -> (c -> [Datum c [a] a (Map a Score)] -> IO ([a],a))
  -> String
  -> Exercise t m c q a e

multipleChoiceExercise answers sound i c cw de g r = Exercise {
  exerciseId = i,
  defaultConfig = c,
  configWidget = cw,
  defaultEvaluation = empty,
  displayEvaluation = de,
  generateQuestion = g,
  questionWidget = multipleChoiceQuestionWidget answers sound,
  reflectiveQuestion = r
}

multipleChoiceQuestionWidget :: (MonadWidget t m, Show a)
  => [a] -- fixed list of potential answers
  -> (Maybe a -> Sound) -- function to produce a sound from an answer (or nothing if reference button pressed)
  -> c
  -> Map a Score
  -> Event t ([a],a)
  -> m (Event t (Datum c [a] a (Map a Score),Event t Sound,Event t ExerciseNavigation))

multipleChoiceQuestionWidget answers sound config initialEval newQuestion = do
  let maxTries = 3::Int

  -- UserMedia widget
  dynFilt <- mapDyn sound answer
  userMediaWidget dynFilt

  -- Managing number of tries
  listOfClicked <- foldDyn ($) [] $ leftmost [fmap (:) bandPressed, (const []) <$ newQuestion]
  let tryEv = attachWithMaybe (\l e -> if elem e l then Nothing else Just e) (current listOfClicked) bandPressed
  tries <- foldDyn ($) 0 $ leftmost [(+1) <$ tryEv, (const 0) <$ nextQuestion]
  canTry <- mapDyn (<maxTries) tries >>= combineDyn (&&) notCorrectYet
  let cannotTryEv = ffilter not $ updated canTry

  -- produce events for correct and incorrect answers

  question <- holdDyn Nothing $ fmap fst newQuestion -- m (Dynamic t (Maybe [a]))
  answer <- holdDyn Nothing $ fmap snd newQuestion -- m (Dynamic t (Maybe a))
  let answerEvent = fmap Just $ gate (current canTry) tryEv -- Event t (Maybe a)

  notCorrectYet <- holdDyn True $ leftmost [True <$ newQuestion, False <$ correctAnswer]

  let correctOrIncorrect = attachDynWith (\a u -> if a==u then Right a else Left u) answer answerEvent   -- Event (Either Frequency Frequency)
  let correctAnswer = fmapMaybe (either (const Nothing) Just) correctOrIncorrect    -- Event t (Maybe a) when correct
  let incorrectAnswer = fmapMaybe (either Just (const Nothing)) correctOrIncorrect  -- Event t (Maybe a) when incorrect
  lastIncorrectAndCorrect <- holdDyn Nothing incorrectAnswer >>= combineDyn (,) answer

  -- use new questions, correct and incorrect answer events to calculate button modes
  let initialModes = fmap (const NotPossible) answers -- [AnswerButtonMode]
  let newQuestionModes = fmap ((\xs -> fmap (flip elem $ xs) answers) . fst) newQuestion

  modes <- foldDyn ($) initialModes $ leftmost [
    (const initialModes) <$ newQuestion,  -- Event t ([AnswerButtonMode] -> [answerButtonMode])
    -- @ make this simpler/neater
    fmap (either (\(x,y)->flip changeModesForCorrectAnswer frequencies x . replaceAtSameIndex y frequencies IncorrectActivated)  (flip changeModesForCorrectAnswer frequencies . fst)) $ leftmost [fmap (\x->Right(x,x)) correctAnswer, attachDynWith (\(x,y) _->Left (x,y)) lastIncorrectAndCorrect cannotTryEv],
    fmap (\x-> replaceAtSameIndex x frequencies IncorrectActivated) incorrectAnswer
  ]
  modes' <- mapM (\x-> mapDyn (!!x) modes) [0,1..9]

  -- buttons
  playUnfiltered <- button "Listen to unfiltered"
  playButton <- button "Play question"
  bandPressed <- elClass "div" "answerButtonWrapper" $ -- m (Event t Frequency)
  leftmost <$> zipWithM (\f m -> answerButton (freqAsString f) m f) frequencies modes'

  -- update scoreMap
  let answerInfo = attachDyn answer correctOrIncorrect  -- Event t (Frequency,Either Frequency Frequency)  (answer, user answer)
  let scoreUpdate = attachWith updateScore (current scoreMap) answerInfo
  scoreMap <- holdDyn defaultEval scoreUpdate

  -- display feedback
  let resetFeedback = fmap (const "") navEvents
  let answerFeedback = fmap (either (const "Incorrect") (const "Correct!")) correctOrIncorrect
  feedbackToDisplay <- holdDyn "" $ leftmost [resetFeedback,answerFeedback]
  dynText feedbackToDisplay

  -- generate sounds to be played
  let playCorrectSound = (\x-> FilteredSound (BufferSource (File "pinknoise.wav") 2.0) (Filter Peaking (freqAsDouble x) 1.4 16.0)) <$> tagDyn answer playButton
  let playOtherSounds = (\x-> FilteredSound (BufferSource (File "pinknoise.wav") 2.0) (Filter Peaking (freqAsDouble x) 1.4 16.0)) <$> bandPressed
  let unfilteredSound = Sound (BufferSource (File "pinknoise.wav") 2.0) <$ playUnfiltered
  let playSounds = leftmost [playCorrectSound,playOtherSounds,unfilteredSound]

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


changeModesForCorrectAnswer::(Eq a)=> a -> [a] -> [AnswerButtonMode] -> [AnswerButtonMode]
changeModesForCorrectAnswer answer possibleAnswers xs = fmap f $ replaceAtSameIndex answer possibleAnswers Correct xs
where
      f Possible = IncorrectDisactivated
      f x = x

prototypeDisplayEvaluation::MonadWidget t m => Dynamic t (M.Map Frequency Score) -> m ()
prototypeDisplayEvaluation = displaySpectrumEvaluation (constDyn "Session Performance")

-- replaces b in [b] at the same index that a is in [a]
replaceAtSameIndex::(Eq a)=>a -> [a] -> b -> [b] -> [b]
replaceAtSameIndex k l mode = maybe id (\x->replaceAt x mode) index
where
  index = elemIndex k l
  replaceAt n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls
