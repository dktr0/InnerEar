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
  defaultEvaluation = replicate 10 (0,0,0),
  configWidget = prototypeConfigWidget,
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
prototypeConfigWidget _ = do
  text "Select at least 2 bands to include in questions:"
  a <- (AllBands <$) <$> button "All Bands"
  b <- (HighBands <$) <$> button "High Bands"
  c <- (MidBands <$) <$> button "Mid Bands"
  d <- (Mid8Bands <$) <$> button "Mid 8 Bands"
  e <- (LowBands <$) <$> button "Low Bands"
  return $ leftmost [a,b,c,d,e]
  -- fcbs <- zipWithM filterCheckBox labels initialConfig -- m [Dynamic t Bool]
  -- config <- listOfDynToDynList fcbs
  -- nextButton <- button "Next"
  -- return $ tagDyn config nextButton

filterCheckBox :: MonadWidget t m => String -> Bool -> m (Dynamic t Bool)
filterCheckBox t v = el "div" $ do
  x <- checkbox v $ def
  text t
  return $ _checkbox_value x

prototypeGenerateQuestion :: WhatBandsAreAllowed -> [Datum WhatBandsAreAllowed [Int] Int ()] -> IO ([Int],Int)
prototypeGenerateQuestion config prevData = do
  let config' = convertBands config
  let x = findIndices (==True) config'
  y <- getStdRandom ((randomR (0,(length x) - 1))::StdGen -> (Int,StdGen))
  return (x,x!!y)

prototypeQuestionWidget :: MonadWidget t m => [(Int,Int,Int)] -> Event t ([Int],Int) -> m (Event t (Datum WhatBandsAreAllowed [Int] Int ()),Event t Sound,Event t ExerciseNavigation)
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

  band0 <- (0 <$) <$> tempWidget (labels!!0) buttonText0
  band1 <- (1 <$) <$> tempWidget (labels!!1) buttonText1
  band2 <- (2 <$) <$> tempWidget (labels!!2) buttonText2
  band3 <- (3 <$) <$> tempWidget (labels!!3) buttonText3
  band4 <- (4 <$) <$> tempWidget (labels!!4) buttonText4
  band5 <- (5 <$) <$> tempWidget (labels!!5) buttonText5
  band6 <- (6 <$) <$> tempWidget (labels!!6) buttonText6
  band7 <- (7 <$) <$> tempWidget (labels!!7) buttonText7
  band8 <- (8 <$) <$> tempWidget (labels!!8) buttonText8
  band9 <- (9 <$) <$> tempWidget (labels!!9) buttonText9
  let bandPressed = leftmost [band0,band1,band2,band3,band4,band5,band6,band7,band8,band9]

  canAnswer <- mapDyn (==Nothing) userAnswer
  let answerEvent = gate (current canAnswer) bandPressed -- Event t Int
  let correctAnswerEvent = attachDynWith (==) correctAnswer answerEvent -- Event t Bool

  -- keeping track of how many times any question answered
  let initialTimesHasBeenAskedThisSessionList = fmap (\(x,_,_) -> x) defaultEval
  let whatWouldHaveBeenCorrect = tagDyn correctAnswer answerEvent -- Event t Int
  let incTimesHasBeenAskedThisSession = fmap (\x -> replaceInList x 1 (replicate 10 0)) whatWouldHaveBeenCorrect
  timesHasBeenAskedThisSession <- foldDyn (+) initialTimesHasBeenAskedThisSessionList incTimesHasBeenAskedThisSession

  -- keeping track of how many times answered exactly correctly
  let indexOfAnsweredCorrectly = tagDyn correctAnswer answeredCorrectly
  let initialTimesHasBeenAnsweredCorrectly = fmap (\(_,x,_) -> x) defaultEval
  let answeredCorrectly = ffilter (==True) correctAnswerEvent
  let incTimesHasBeenAnsweredCorrectly = fmap (\x -> replaceInList x 1 (replicate 10 0)) indexOfAnsweredCorrectly
  timesHasBeenAnsweredCorrectly <- foldDyn (+) initialTimesHasBeenAnsweredCorrectly incTimesHasBeenAnsweredCorrectly

  -- keeping track of how many times answered one band up or down
  let initialTimesHasBeenOneUpOrDown = fmap (\(_,_,x) -> x) defaultEval
  let oneUpOrDown = attachDynWith (\x y -> (y==(x-1)) || (y==(x+1))) correctAnswer answerEvent -- Event t Bool
  let oneUpOrDownIndex = tagDyn correctAnswer $ ffilter (==True) oneUpOrDown
  let incTimesHasBeenOneUpOrDown = fmap (\x -> replaceInList x 1 (replicate 10 0)) indexOfAnsweredCorrectly
  timesHasBeenOneUpOrDown <- foldDyn (+) initialTimesHasBeenOneUpOrDown incTimesHasBeenOneUpOrDown

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


tempWidget :: MonadWidget t m => String -> Dynamic t (Maybe String) -> m (Event t ())
tempWidget labelText buttonText = el "div" $ do
  text labelText
  buttonText' <- mapDyn (maybe "" id) buttonText
  spacerOrButton <- mapDyn (maybe False (const True)) buttonText
  flippableDynE mySpacer (dynButton buttonText') spacerOrButton

mySpacer :: MonadWidget t m => m (Event t ())
mySpacer = button "-"
