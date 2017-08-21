{-# LANGUAGE RecursiveDo, DeriveDataTypeable #-}

-- | We only export a single definition from an Exercise module in Inner Ear,
-- that definition being a single value of the parameterized type Exercise.

module InnerEar.Exercises.Prototype (prototypeExercise) where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import System.Random
import Data.Maybe (fromJust)
import Data.Bool (bool)
import Data.List (findIndices)
import Text.JSON
import Text.JSON.Generic
import Data.List(elemIndex)

import InnerEar.Widgets.Utility
import InnerEar.Types.Data
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Widgets.Bars
import InnerEar.Widgets.Test
import InnerEar.Types.Exercise
import InnerEar.Types.ExerciseId
import InnerEar.Types.ExerciseNavigation

-- | We need to carefully and explicitly give a type signature with the definition
-- of each specific exercise value. In the parameters after Exercise in the type signature
-- below, the t and the m are constraints due to our use of Reflex-Dom widgets. Then:
--
-- Our configuration is currently a list of bools representing which of 10 bands are possible.
-- Our internal question format is a list of ints representing the index of bands included in the question.
-- Our internal answer format is an int representing the index of the correct answer to the question.
-- And we currently have no evaluation format so have left it at ().

data WhatBandsAreAllowed = AllBands | HighBands | MidBands | Mid8Bands | LowBands deriving (Show,Eq,Data,Typeable)

convertBands :: WhatBandsAreAllowed -> [Bool]
convertBands AllBands = replicate 10 True
convertBands HighBands = [False,False,False,False,False,True,True,True,True,True]
convertBands MidBands = [False,False,False,True,True,True,True,True,False,False]
convertBands Mid8Bands = [False,True,True,True,True,True,True,True,True,False]
convertBands LowBands = [False,False,False,False,False,True,True,True,True,True]

prototypeExercise :: MonadWidget t m => Exercise t m WhatBandsAreAllowed [Int] Int [(Int,Int,Int)]
prototypeExercise = Exercise {
  exerciseId = PrototypeExercise,
  defaultConfig = AllBands,
  configWidget = prototypeConfigWidget,
  defaultEvaluation = replicate 10 (0,0,0),
  displayEvaluation = prototypeDisplayEvaluation,
  generateQuestion = prototypeGenerateQuestion,
  questionWidget = prototypeQuestionWidget,
  reflectiveQuestion = Just "Please write some brief text reflecting on your experience:"
  }

-- | Because we only export the one definition above from this module, we can create other definitions
-- with whatever names we like, with great abandon!

filters:: [Filter]
filters = fmap (\x -> Filter Peaking x 1.4 16.0) [31,63,125,250,500,1000,2000,4000,8000,16000]

sounds :: [Sound]
sounds = fmap (FilteredSound (BufferSource (File "pinknoise.wav") 2.0)) filters

labels :: [String]
labels = ["31 Hz","63 Hz","125 Hz","250 Hz","500 Hz","1 kHz","2 kHz","4 kHz","8 kHz","16 kHz"]

prototypeConfigWidget :: MonadWidget t m => WhatBandsAreAllowed -> m (Event t WhatBandsAreAllowed)
prototypeConfigWidget i = do
  let radioButtonMap =  zip [0::Int,1..] [AllBands,HighBands,MidBands,Mid8Bands,LowBands]
  let iVal = maybe 0 id $ elemIndex i [AllBands,HighBands,MidBands,Mid8Bands,LowBands]
  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ fmap (\(x,y)->(x,show y)) radioButtonMap)
           (WidgetConfig {_widgetConfig_initialValue= Just iVal
                         ,_widgetConfig_setValue = never
                         ,_widgetConfig_attributes = constDyn M.empty})

  return $ fmap (\x-> maybe AllBands id $ M.lookup (maybe 0 id x) (M.fromList radioButtonMap)) (_hwidget_change radioWidget)
  --userAnswer <- holdDyn Nothing $ tagDyn (_hwidget_value radioWidget)

  -- fcbs <- zipWithM filterCheckBox labels initialConfig -- m [Dynamic t Bool]
  -- config <- listOfDynToDynList fcbs
  -- nextButton <- button "Next"
  -- return $ tagDyn config nextButton

filterCheckBox :: MonadWidget t m => String -> Bool -> m (Dynamic t Bool)
filterCheckBox t v = el "div" $ do
  x <- checkbox v $ def
  text t
  return $ _checkbox_value x



prototypeGenerateQuestion :: WhatBandsAreAllowed -> [Datum WhatBandsAreAllowed [Int] Int [(Int, Int, Int)]] -> IO ([Int],Int)
prototypeGenerateQuestion config prevData = do
  let config' = convertBands config
  let x = findIndices (==True) config'
  y <- getStdRandom ((randomR (0,(length x) - 1))::StdGen -> (Int,StdGen))
  return (x,x!!y)





prototypeQuestionWidget :: MonadWidget t m => [(Int,Int,Int)] -> Event t ([Int],Int) -> m (Event t (Datum WhatBandsAreAllowed [Int] Int [(Int, Int, Int)]), Event t Sound, Event t ExerciseNavigation)
prototypeQuestionWidget defaultEval e = mdo

  question <- holdDyn ([],0) e
  correctAnswer <- mapDyn snd question
  userAnswer <- holdDyn Nothing $ leftmost [Nothing <$ e,Just <$> answerEvent]

  playUnfiltered <- button "Listen to unfiltered"
  playButton <- button "Listen to question"

  let unfilteredSound = Sound (BufferSource (File "pinknoise.wav") 2.0) <$ playUnfiltered

  band0included <- mapDyn (elem 0 . fst) question
  band1included <- mapDyn (elem 1 . fst) question
  band2included <- mapDyn (elem 2 . fst) question
  band3included <- mapDyn (elem 3 . fst) question
  band4included <- mapDyn (elem 4 . fst) question
  band5included <- mapDyn (elem 5 . fst) question
  band6included <- mapDyn (elem 6 . fst) question
  band7included <- mapDyn (elem 7 . fst) question
  band8included <- mapDyn (elem 8 . fst) question
  band9included <- mapDyn (elem 9 . fst) question

  buttonText0 <- combineDyn f userAnswer band0included
  buttonText1 <- combineDyn f userAnswer band1included
  buttonText2 <- combineDyn f userAnswer band2included
  buttonText3 <- combineDyn f userAnswer band3included
  buttonText4 <- combineDyn f userAnswer band4included
  buttonText5 <- combineDyn f userAnswer band5included
  buttonText6 <- combineDyn f userAnswer band6included
  buttonText7 <- combineDyn f userAnswer band7included
  buttonText8 <- combineDyn f userAnswer band8included
  buttonText9 <- combineDyn f userAnswer band9included

  bandPressed <- elClass "div" "barWrapper" $ do
    band0 <- (0 <$) <$> dynLabelBarButton  (labels!!0) band0count buttonText0 band0perf
    band1 <- (1 <$) <$> dynLabelBarButton  (labels!!1) band1count buttonText1 band1perf
    band2 <- (2 <$) <$> dynLabelBarButton  (labels!!2) band2count buttonText2 band2perf
    band3 <- (3 <$) <$> dynLabelBarButton  (labels!!3) band3count buttonText3 band3perf
    band4 <- (4 <$) <$> dynLabelBarButton  (labels!!4) band4count buttonText4 band4perf
    band5 <- (5 <$) <$> dynLabelBarButton  (labels!!5) band5count buttonText5 band5perf
    band6 <- (6 <$) <$> dynLabelBarButton (labels!!6) band6count buttonText6 band6perf
    band7 <- (7 <$) <$> dynLabelBarButton  (labels!!7) band7count buttonText7 band7perf
    band8 <- (8 <$) <$> dynLabelBarButton (labels!!8) band8count buttonText8 band8perf
    band9 <- (9 <$) <$> dynLabelBarButton  (labels!!9) band9count buttonText9 band9perf
    return $ leftmost [band0,band1,band2,band3,band4,band5,band6,band7,band8,band9]

  canAnswer <- mapDyn (==Nothing) userAnswer
  let answerEvent = gate (current canAnswer) bandPressed -- Event t Int
  let correctAnswerEvent = attachDynWith (==) correctAnswer answerEvent -- Event t Bool

  -- keeping track of how many times any question answered
  let initialTimesHasBeenAskedThisSessionList = fmap (\(x,_,_) -> x) defaultEval
  let whatWouldHaveBeenCorrect = tagDyn correctAnswer answerEvent -- Event t Int
  let incTimesHasBeenAskedThisSession = fmap (\x -> replaceInList x (1::Int) (replicate 10 (0::Int))) whatWouldHaveBeenCorrect
  timesHasBeenAskedThisSession <- foldDyn (zipWith (+)) initialTimesHasBeenAskedThisSessionList incTimesHasBeenAskedThisSession

  -- keeping track of how many times answered exactly correctly
  let indexOfAnsweredCorrectly = tagDyn correctAnswer answeredCorrectly
  let initialTimesHasBeenAnsweredCorrectly = fmap (\(_,x,_) -> x) defaultEval
  let answeredCorrectly = ffilter (==True) correctAnswerEvent
  let incTimesHasBeenAnsweredCorrectly = fmap (\x -> replaceInList x (1::Int) (replicate 10 0)) indexOfAnsweredCorrectly
  timesHasBeenAnsweredCorrectly <- foldDyn (zipWith (+)) initialTimesHasBeenAnsweredCorrectly incTimesHasBeenAnsweredCorrectly

  -- keeping track of how many times answered one band up or down
  let initialTimesHasBeenOneUpOrDown = fmap (\(_,_,x) -> x) defaultEval
  let oneUpOrDown = attachDynWith (\x y -> (y==(x-1)) || (y==(x+1))) correctAnswer answerEvent -- Event t Bool
  let oneUpOrDownIndex = tagDyn correctAnswer $ ffilter (==True) oneUpOrDown
  let incTimesHasBeenOneUpOrDown = fmap (\x -> replaceInList x 1 (replicate 10 0)) indexOfAnsweredCorrectly
  timesHasBeenOneUpOrDown <- foldDyn (zipWith (+)) initialTimesHasBeenOneUpOrDown incTimesHasBeenOneUpOrDown

  band0count <- mapDyn (Just . (!!0)) timesHasBeenAskedThisSession
  band1count <- mapDyn (Just . (!!1)) timesHasBeenAskedThisSession
  band2count <- mapDyn (Just . (!!2)) timesHasBeenAskedThisSession
  band3count <- mapDyn (Just . (!!3)) timesHasBeenAskedThisSession
  band4count <- mapDyn (Just . (!!4)) timesHasBeenAskedThisSession
  band5count <- mapDyn (Just . (!!5)) timesHasBeenAskedThisSession
  band6count <- mapDyn (Just . (!!6)) timesHasBeenAskedThisSession
  band7count <- mapDyn (Just . (!!7)) timesHasBeenAskedThisSession
  band8count <- mapDyn (Just . (!!8)) timesHasBeenAskedThisSession
  band9count <- mapDyn (Just . (!!9)) timesHasBeenAskedThisSession

  band0correct <- mapDyn (!!0) timesHasBeenAnsweredCorrectly
  band1correct <- mapDyn (!!1) timesHasBeenAnsweredCorrectly
  band2correct <- mapDyn (!!2) timesHasBeenAnsweredCorrectly
  band3correct <- mapDyn (!!3) timesHasBeenAnsweredCorrectly
  band4correct <- mapDyn (!!4) timesHasBeenAnsweredCorrectly
  band5correct <- mapDyn (!!5) timesHasBeenAnsweredCorrectly
  band6correct <- mapDyn (!!6) timesHasBeenAnsweredCorrectly
  band7correct <- mapDyn (!!7) timesHasBeenAnsweredCorrectly
  band8correct <- mapDyn (!!8) timesHasBeenAnsweredCorrectly
  band9correct <- mapDyn (!!9) timesHasBeenAnsweredCorrectly

  band0perf <- combineDyn calculateScore band0correct band0count
  band1perf <- combineDyn calculateScore band1correct band1count
  band2perf <- combineDyn calculateScore band2correct band2count
  band3perf <- combineDyn calculateScore band3correct band3count
  band4perf <- combineDyn calculateScore band4correct band4count
  band5perf <- combineDyn calculateScore band5correct band5count
  band6perf <- combineDyn calculateScore band6correct band6count
  band7perf <- combineDyn calculateScore band7correct band7count
  band8perf <- combineDyn calculateScore band8correct band8count
  band9perf <- combineDyn calculateScore band9correct band9count



  mapDyn show timesHasBeenAskedThisSession >>= dynText
  mapDyn show timesHasBeenAnsweredCorrectly >>= dynText
  mapDyn show timesHasBeenOneUpOrDown >>= dynText

  -- display feedback
  let resetFeedback = fmap (const "") e
  let answerFeedback = fmap (bool "Incorrect" "Correct!") correctAnswerEvent
  feedbackToDisplay <- holdDyn "" $ leftmost [resetFeedback,answerFeedback]
  dynText feedbackToDisplay

  -- generate sounds to be played
  let playCorrectSound = (sounds!!) <$> tagDyn correctAnswer playButton
  let playOtherSounds = (sounds!!) <$> bandPressed
  let playSounds = leftmost [playCorrectSound,playOtherSounds,unfilteredSound]

  -- generate navigation events
  backToConfigure <- (InConfigure <$) <$> button "Configure"
  nextQuestion <- (InQuestion <$) <$> button "New Question"
  onToReflect <- (InReflect <$) <$> button "Reflect"
  let navEvents = leftmost [backToConfigure,nextQuestion,onToReflect]

  return (never,playSounds,navEvents)
  where
    f (Nothing) True = Just "?"
    f _ True = Just "L"
    f _ False = Nothing

calculateScore :: Int -> Maybe Int -> Maybe Float
calculateScore x (Just y) = Just (fromIntegral(x)/fromIntegral(y))
calculateScore x _ = Just (-1.0)

{-
tempWidget :: MonadWidget t m => String -> Dynamic t (Maybe String) -> m (Event t ())
tempWidget labelText buttonText = el "div" $ do
  text labelText
  buttonText' <- mapDyn (maybe "" id) buttonText
  spacerOrButton <- mapDyn (maybe False (const True)) buttonText
  flippableDynE mySpacer (dynButton buttonText') spacerOrButton
-}

mySpacer :: MonadWidget t m => m (Event t ())
mySpacer = button "-"
