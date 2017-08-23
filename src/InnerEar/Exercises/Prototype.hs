{-# LANGUAGE RecursiveDo, DeriveDataTypeable #-}

-- | We only export a single definition from an Exercise module in Inner Ear,
-- that definition being a single value of the parameterized type Exercise.

--module InnerEar.Exercises.Prototype (prototypeExercise) where
module InnerEar.Exercises.Prototype where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable,hashWithSalt)
import System.Random
import Data.Maybe (fromJust)
import Data.Bool (bool)
import Data.List (findIndices,partition)
import Text.JSON
import Text.JSON.Generic
import Data.List(elemIndex)

import qualified InnerEar.Widgets.AnswerButton as AB
import InnerEar.Widgets.Utility
import InnerEar.Types.Data
import InnerEar.Types.Score
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Widgets.Bars
import InnerEar.Widgets.Test
import InnerEar.Types.Exercise
import InnerEar.Types.ExerciseId
import InnerEar.Types.ExerciseNavigation
import InnerEar.Types.Frequency

-- | We need to carefully and explicitly give a type signature with the definition
-- of each specific exercise value. In the parameters after Exercise in the type signature
-- below, the t and the m are constraints due to our use of Reflex-Dom widgets. Then:
--
-- Our configuration is currently a list of bools representing which of 10 bands are possible.
-- Our internal question format is a list of ints representing the index of bands included in the question.
-- Our internal answer format is an int representing the index of the correct answer to the question.
-- And we currently have no evaluation format so have left it at ().

data WhatBandsAreAllowed = AllBands | HighBands | MidBands | Mid8Bands | LowBands deriving (Show,Eq,Data,Typeable)

instance Hashable WhatBandsAreAllowed where
  hashWithSalt _ AllBands = 1
  hashWithSalt _ HighBands = 2
  hashWithSalt _ MidBands= 3
  hashWithSalt _ Mid8Bands = 4
  hashWithSalt _ LowBands= 5


-- So e can be a Map from allowedbands to BandsEval
instance Ord WhatBandsAreAllowed where
  compare AllBands b = if b==AllBands then EQ else GT
  compare HighBands b = case b of
    (AllBands) -> LT
    (HighBands) -> EQ
    otherwise -> GT
  compare (Mid8Bands) b = case b of
    (MidBands) -> GT
    (LowBands) -> GT
    (Mid8Bands) -> EQ
    otherwise -> LT
  compare (MidBands) b = case b of
    (LowBands) -> GT
    (MidBands) -> EQ
    otherwise -> LT
  compare (LowBands) b = if b==LowBands then EQ else LT


-- Evaluation for a particular configuration. Map of hz to a Band Evaluation (could also be from Double to BandEval if want decimal hz)
type ConfigEval = M.Map Int Score


convertBands :: WhatBandsAreAllowed -> [Bool]
convertBands AllBands = replicate 10 True
convertBands HighBands = [False,False,False,False,False,True,True,True,True,True]
convertBands MidBands = [False,False,False,True,True,True,True,True,False,False]
convertBands Mid8Bands = [False,True,True,True,True,True,True,True,True,False]
convertBands LowBands = [True,True,True,True,True,False,False,False,False,False]

prototypeExercise :: MonadWidget t m => Exercise t m WhatBandsAreAllowed [Frequency] Frequency (M.Map Frequency Score)
prototypeExercise = Exercise {
  exerciseId = PrototypeExercise,
  defaultConfig = AllBands,
  configWidget = prototypeConfigWidget,
  defaultEvaluation = M.empty,
  displayEvaluation = prototypeDisplayEvaluation,
  generateQuestion = prototypeGenerateQuestion,
  questionWidget = prototypeQuestionWidget,
  reflectiveQuestion = Just "Please write some brief text reflecting on your experience:"
  }

-- | Because we only export the one definition above from this module, we can create other definitions
-- with whatever names we like, with great abandon!

frequencies :: [Frequency]
frequencies = [
  F 31 "31", F 63 "63", F 125 "125", F 250 "250", F 500 "500",
  F 1000 "1k", F 2000 "2k", F 4000 "4k", F 8000 "8k", F 16000 "16k"]


prototypeConfigWidget :: MonadWidget t m => WhatBandsAreAllowed -> m (Event t WhatBandsAreAllowed)
prototypeConfigWidget i = do
  let radioButtonMap =  zip [0::Int,1..] [AllBands,HighBands,MidBands,Mid8Bands,LowBands]
  let iVal = maybe 0 id $ elemIndex i [AllBands,HighBands,MidBands,Mid8Bands,LowBands]
  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ fmap (\(x,y)->(x,show y)) radioButtonMap)
           (WidgetConfig {_widgetConfig_initialValue= Just iVal
                         ,_widgetConfig_setValue = never
                         ,_widgetConfig_attributes = constDyn M.empty})
  dynConfig <- holdDyn AllBands $ fmap (\x-> maybe AllBands id $ M.lookup (maybe 0 id x) (M.fromList radioButtonMap)) (_hwidget_change radioWidget)
  b <- button "Continue to Exercise"
  return $ tagDyn dynConfig b


prototypeGenerateQuestion :: WhatBandsAreAllowed -> [Datum WhatBandsAreAllowed [Frequency] Frequency (M.Map Frequency Score)] -> IO ([Frequency],Frequency)
prototypeGenerateQuestion config prevData = do
  let config' = convertBands config
  let x = fmap (\i-> frequencies!!i) $ findIndices (==True) config'
  y <- getStdRandom ((randomR (0,(length x) - 1))::StdGen -> (Int,StdGen))
  return (x,x!!y)


prototypeQuestionWidget :: MonadWidget t m
  => WhatBandsAreAllowed
  -> M.Map Frequency Score
  -> Event t ([Frequency],Frequency)
  -> m (Event t (Datum WhatBandsAreAllowed [Frequency] Frequency (M.Map Frequency Score)), Event t Sound, Event t ExerciseNavigation)

prototypeQuestionWidget config defaultEval newQuestion = mdo
  let maxTries = 3::Int

  -- calculate tries so far

  --listOfClicked <- foldDyn ($) [] $ leftmost [fmap (x-> (maybe (-1) id $ elemIndex x frequencies):) bandPressed, (const []) <$ newQuestion]
  listOfClicked <- foldDyn ($) [] $ leftmost [fmap (:) bandPressed, (const []) <$ newQuestion]
  let tryEv = attachWithMaybe (\l e -> if elem e l then Nothing else Just e) (current listOfClicked) bandPressed
  
  --tries <- foldDyn ($) 0 $ leftmost [(+1) bandPressed, (const 0) <$ nextQuestion]

  tries <- foldDyn ($) 0 $ leftmost [(+1) <$ tryEv, (const 0) <$ nextQuestion]
  canTry <- mapDyn (<maxTries) tries >>= combineDyn (&&) notCorrectYet
  let cannotTryEv = ffilter not $ updated canTry
  --let cannotTryEv = fmap not $ updated canTry
  --let cannotTryEv' = attachDynWith (const $ fmap (\x->case x of)) answer cannotTryEv
  holdDyn "test" (fmap show cannotTryEv) >>= dynText

  -- produce events for correct and incorrect answers
  question <- holdDyn ([],F 31 "31") newQuestion
  answer <- mapDyn snd question  -- Dyn t Frequency
  let answerEvent = gate (current canTry) tryEv -- Event t Frequency
  
  notCorrectYet <- holdDyn True $ leftmost [True <$ newQuestion, False <$ correctAnswer]
  --userAnswer <- holdDyn Nothing $ leftmost [Nothing <$ newQuestion,Just <$> answerEvent]
  --canAnswer <- mapDyn (==Nothing) userAnswer
  --canAnswer <- mapDyn (==Nothing) userAnswer >>= combineDyn (&&) canTry

  let correctOrIncorrect = attachDynWith (\a u -> if a==u then Right a else Left u) answer answerEvent   -- Event (Either Frequency Frequency)
  let correctAnswer = fmapMaybe (either (const Nothing) Just) correctOrIncorrect    -- Event t Frequency
  let incorrectAnswer = fmapMaybe (either Just (const Nothing)) correctOrIncorrect  -- Event t Frequency if incorrect, no event if correct

  -- use new questions, correct and incorrect answer events to calculate button modes
  let initialModes = fmap (bool AB.NotPossible AB.Possible) $ convertBands config

  modes <- foldDyn ($) initialModes $ leftmost [
    (const initialModes) <$ newQuestion,                         -- Event t ([AnswerButtonMode] -> [answerButtonMode])
    fmap (flip changeModesForCorrectAnswer frequencies) correctAnswer,
    fmap (\x-> replaceAtSameIndex x frequencies AB.IncorrectDisactivated) incorrectAnswer,
    (fmap (\x-> case x of AB.IncorrectActivated-> AB.IncorrectDisactivated; otherwise->x)) <$ cannotTryEv  --Once tries are up,
    ]

  --modes' <- zipWithM (\x y -> mapDyn (!!y) x) (repeat modes) [0,1..9]
  modes' <- mapM (\x-> mapDyn (!!x) modes) [0,1..9]
  -- buttons
  playUnfiltered <- button "Listen to unfiltered"
  playButton <- button "Play question"
  bandPressed <- elClass "div" "answerButtonWrapper" $ -- m (Event t Frequency)
    leftmost <$> zipWithM (\f m -> AB.answerButton (constDyn $ show f) m f) frequencies modes'

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
  backToConfigure <- (InConfigure <$) <$> button "Configure"
  nextQuestion <- (InQuestion <$) <$> button "New Question"
  onToReflect <- (InReflect <$) <$> button "Reflect"
  let navEvents = leftmost [backToConfigure,nextQuestion,onToReflect]

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

  return (never, playSounds,navEvents)


--changeModesForCorrectAnswer :: Int -> [AnswerButtonMode] -> [AnswerButtonMode]
--changeModesForCorrectAnswer i xs = fmap f $ replaceInList i AB.Correct xs
--  where f AB.IncorrectDisactivated = AB.IncorrectActivated
--        f x = x

--replaceInList:: Int -> AnswerButtonMode -> [AnswerButtonMode] -> [AnswerButtonMode]


changeModesForCorrectAnswer::(Eq a)=> a -> [a] -> [AB.AnswerButtonMode] -> [AB.AnswerButtonMode]
changeModesForCorrectAnswer answer possibleAnswers xs = fmap f $ replaceAtSameIndex answer possibleAnswers AB.Correct xs
  where f AB.IncorrectDisactivated = AB.IncorrectActivated
        f x = x

prototypeDisplayEvaluation::MonadWidget t m => Dynamic t (M.Map Frequency Score) -> m ()
prototypeDisplayEvaluation e = return ()


-- replaces b in [b] at the same index that a is in [a]
replaceAtSameIndex::(Eq a)=>a -> [a] -> b -> [b] -> [b]
replaceAtSameIndex k l mode = maybe id (\x->replaceAt x mode) index
  where
    index = elemIndex k l
    replaceAt n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls



--    buttons::MonadWidget t m => [a] -> Dynamic t [AnswerButtonMode] -> Event t a
--    buttons::MonadWidget t m => Dynamic t [(a,AnswerButtonMode)] -> m (Event t a)
--    buttons l = do
--      dynMap <- mapDyn fromList l
--      evMap <- listViewWithKey dynMap AB.answerButton 


--answerButton:: MonadWidget t m => a -> Dynamic t AnswerButtonMode  -> m (Event t a)


--toListDyn::Dynamic [] ->[Dyn]


--assume that w is Dynamic t [a]
--x <- mapDyn (!!0) w :: m (Dynamic t a)
--y <- mapDyn (!!1) w :: m (Dynamic t a)

--let z = [x,y] :: [Dynamic t a]

--listViewWithKey::Dynamic (Map k v) -> (k -> Dynamic v -> m (Event a)) -> m (Event   (Map k a))
