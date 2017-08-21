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
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable,hashWithSalt)
import System.Random
import Data.Maybe (fromJust)
import Data.Bool (bool)
import Data.List (findIndices,partition)
import Text.JSON
import Text.JSON.Generic
import Data.List(elemIndex)

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

type TenBandsEval = HM.HashMap WhatBandsAreAllowed ConfigEval



convertBands :: WhatBandsAreAllowed -> [Bool]
convertBands AllBands = replicate 10 True
convertBands HighBands = [False,False,False,False,False,True,True,True,True,True]
convertBands MidBands = [False,False,False,True,True,True,True,True,False,False]
convertBands Mid8Bands = [False,True,True,True,True,True,True,True,True,False]
convertBands LowBands = [False,False,False,False,False,True,True,True,True,True]




prototypeExercise :: MonadWidget t m => Exercise t m WhatBandsAreAllowed [Int] Int (M.Map Int Score)
prototypeExercise = Exercise {
  exerciseId = PrototypeExercise,
  defaultConfig = AllBands,
  configWidget = prototypeConfigWidget,
  defaultEvaluation = M.empty,
  --defaultEvaluation = HM.fromList $ zipWith (\c v ->(c, M.fromList $ fmap ((\x->(x,Score 0 0 0))  . snd) $ fst $ partition fst $ zip (convertBands c) v)) [AllBands,HighBands,Mid8Bands, MidBands,LowBands] (repeat [31::Int,63,125,250,500,1000,2000,4000,8000,16000]), -- ugly yet beautiful...
  displayEvaluation = prototypeDisplayEvaluation,
  generateQuestion = prototypeGenerateQuestion,
  questionWidget = prototypeQuestionWidget,
  reflectiveQuestion = Just "Please write some brief text reflecting on your experience:"
  }


-- | Because we only export the one definition above from this module, we can create other definitions
-- with whatever names we like, with great abandon!

filters:: [Filter]
filters = fmap (\x -> Filter Peaking x 1.4 16.0) filterFreqs

--filterFreqs::[Int]
filterFreqs = [31,63,125,250,500,1000,2000,4000,8000,16000]

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



prototypeGenerateQuestion :: WhatBandsAreAllowed -> [Datum WhatBandsAreAllowed [Int] Int (M.Map Int Score)] -> IO ([Int],Int)
prototypeGenerateQuestion config prevData = do
  let config' = convertBands config
  let x = findIndices (==True) config'
  y <- getStdRandom ((randomR (0,(length x) - 1))::StdGen -> (Int,StdGen))
  return (x,x!!y)






prototypeQuestionWidget :: MonadWidget t m => WhatBandsAreAllowed -> M.Map Int Score -> Event t ([Int],Int) -> m (Event t (Datum WhatBandsAreAllowed [Int] Int (M.Map Int Score)), Event t Sound, Event t ExerciseNavigation)
prototypeQuestionWidget c defaultEval e = mdo

  question <- holdDyn ([],0) e
  -- @ questions not generated properly for this rn
  correctAnswer <- mapDyn snd question
  userAnswer <- holdDyn Nothing $ leftmost [Nothing <$ e,Just <$> answerEvent]

  playUnfiltered <- button "Listen to unfiltered"
  playButton <- button "Listen to question"
  let unfilteredSound = Sound (BufferSource (File "pinknoise.wav") 2.0) <$ playUnfiltered

  bandPressed <- elClass "div" "answerButtonWrapper" $ do       -- Event t Int (int - hz corresponding to the band)
    let x = fmap (\x-> buttonVal (show x ++" Hz") x) (fmap snd $ fst $ partition fst $ zip (convertBands c) filterFreqs) -- [ m(Event t Int) ]
    x' <- sequence x
    return $ fmap round $ leftmost x'
 


  canAnswer <- mapDyn (==Nothing) userAnswer
  let answerEvent = gate (current canAnswer) bandPressed -- Event t Int
  let correctAnswerEvent = attachDynWith (==) correctAnswer answerEvent -- Event t Bool

--holdDyn "nothing yet.." (fmap show answerEvent) >>= dynText

  -- update the scoreMap
  let answerInfo = attachDynWith (\cor user -> if cor==user then ([(Correct,cor)],cor) else ([(FalsePositive,user), (FalseNegative,cor)],cor)) correctAnswer answerEvent -- Event t (cor,user)
  let scoreUpdate = attachWith (\s (xs,cor) -> foldl (\b (a,i)-> M.insert i (adjustScore a (maybe (Score 0 0 0) id $ M.lookup i b)) b) s xs) (current scoreMap) answerInfo       
  --let scoreUpdate = attachWith (\s (xs,cor) -> foldl (\b (a,i)-> M.update (Just . adjustScore a) i b) s xs) (current scoreMap) answerInfo       
  
  --M.insertWith (\newVal mapVal -> newVal)

  scoreMap <- holdDyn defaultEval scoreUpdate

  -- display feedback
  let resetFeedback = fmap (const "") e
  let answerFeedback = fmap (bool "Incorrect" "Correct!") correctAnswerEvent
  feedbackToDisplay <- holdDyn "" $ leftmost [resetFeedback,answerFeedback]
  dynText feedbackToDisplay

  -- generate sounds to be played
  let playCorrectSound = (\x-> FilteredSound (BufferSource (File "pinknoise.wav") 2.0) (Filter Peaking (fromIntegral x) 1.4 16.0)) <$> tagDyn correctAnswer playButton
  let playOtherSounds = (\x-> FilteredSound (BufferSource (File "pinknoise.wav") 2.0) (Filter Peaking (fromIntegral x) 1.4 16.0)) <$> tagDyn correctAnswer bandPressed
  let playSounds = leftmost [playCorrectSound,playOtherSounds,unfilteredSound]

  -- generate navigation events
  backToConfigure <- (InConfigure <$) <$> button "Configure"
  nextQuestion <- (InQuestion <$) <$> button "New Question"
  onToReflect <- (InReflect <$) <$> button "Reflect"
  let navEvents = leftmost [backToConfigure,nextQuestion,onToReflect]

  el "div" $ do
    text "debugging:   "
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



calculateScore :: Int -> Maybe Int -> Maybe Float
calculateScore x (Just y) = Just (fromIntegral(x)/fromIntegral(y))
calculateScore x _ = Just (-1.0)


prototypeDisplayEvaluation::MonadWidget t m => Dynamic t (M.Map Int Score) -> m ()
prototypeDisplayEvaluation e = return ()

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
