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


prototypeGenerateQuestion :: WhatBandsAreAllowed -> [Datum WhatBandsAreAllowed [Int] Int (M.Map Int Score)] -> IO ([Int],Int)
prototypeGenerateQuestion config prevData = do
  let config' = convertBands config
  let x = findIndices (==True) config'
  y <- getStdRandom ((randomR (0,(length x) - 1))::StdGen -> (Int,StdGen))
  return (x,x!!y)


prototypeQuestionWidget :: MonadWidget t m
  => WhatBandsAreAllowed
  -> M.Map Frequency Score
  -> Event t ([Frequency],Frequency)
  -> m (Event t (Datum WhatBandsAreAllowed [Frequency] Frequency (M.Map Frequency Score)), Event t Sound, Event t ExerciseNavigation)

prototypeQuestionWidget config defaultEval newQuestion = mdo

  -- produce events for correct and incorrect answers
  question <- holdDyn ([],0) newQuestion
  answer <- mapDyn snd question
  let answerEvent = gate (current canAnswer) bandPressed -- Event t Frequency
  userAnswer <- holdDyn Nothing $ leftmost [Nothing <$ newQuestion,Just <$> answerEvent]
  canAnswer <- mapDyn (==Nothing) userAnswer
  let correctOrIncorrect = attachDynWith (\a u -> if a==u then Right a else Left u) answer answerEvent
  let correctAnswer = fmapMaybe (either (const Nothing) Just) correctOrIncorrect
  let incorrectAnswer = fmapMaybe (either Just (const Nothing)) correctOrIncorrect

  -- use new questions, correct and incorrect answer events to calculate button modes
  let initialModes = fmap (bool NotPossible Possible) $ convertBands config
  modes <- foldDyn initialModes $ leftmost [
    fmap (const initialModes) newQuestion,
    fmap changeModesForCorrectAnswer correctAnswer,
    fmap (flip replaceInList $ IncorrectDisactivated) incorrectAnswer
  ]

  -- buttons
  playUnfiltered <- button "Listen to unfiltered"
  bandPressed <- elClass "div" "answerButtonWrapper" $ -- m (Event t Frequency)
    leftmost <$> zipWithM (\f m -> answerButtonVal (constDyn $ show f) m f) frequencies modes

  -- update the scoreMap
  let answerInfo = attachDynWith (\cor user -> if cor==user then ([(Correct,cor)],cor) else ([(FalsePositive,user), (FalseNegative,cor)],cor)) answer answerEvent -- Event t (cor,user)
  let scoreUpdate = attachWith (\s (xs,cor) -> foldl (\b (a,i)-> M.insert i (adjustScore a (maybe (Score 0 0 0) id $ M.lookup i b)) b) s xs) (current scoreMap) answerInfo
  scoreMap <- holdDyn defaultEval scoreUpdate

  -- display feedback
  let resetFeedback = fmap (const "") e
  let answerFeedback = fmap (bool "Incorrect" "Correct!") correctAnswerEvent
  feedbackToDisplay <- holdDyn "" $ leftmost [resetFeedback,answerFeedback]
  dynText feedbackToDisplay

  -- generate sounds to be played
  let playCorrectSound = (\x-> FilteredSound (BufferSource (File "pinknoise.wav") 2.0) (Filter Peaking (fromIntegral x) 1.4 16.0)) <$> tagDyn answer playButton
  let playOtherSounds = (\x-> FilteredSound (BufferSource (File "pinknoise.wav") 2.0) (Filter Peaking (fromIntegral x) 1.4 16.0)) <$> tagDyn answer bandPressed
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
      text "button states: "
      mapDyn show buttonStates >>= dynText
    el "div"$ do
      text "correct answer:  "
      mapDyn show correctAnswer >>= dynText
    el "div" $ do
      text "canAnswer: "
      mapDyn show canAnswer >>= dynText
    el "div"$ do
      text "userAnswer:  "
      holdDyn "nothing" (fmap show bandPressed) >>= dynText
    el "div"$ do
      text "Score Map:  "
      mapDyn show scoreMap >>= dynText

  return (never, playSounds,navEvents)


changeModesForCorrectAnswer :: Int -> [AnswerButtonMode] -> [AnswerButtonMode]
changeModesForCorrectAnswer i xs = fmap f $ replaceInList i Correct
  where f IncorrectDisactivated = IncorrectActivated
        f x = x


prototypeDisplayEvaluation::MonadWidget t m => Dynamic t (M.Map Int Score) -> m ()
prototypeDisplayEvaluation e = return ()
