{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.Prototype (prototypeExercise) where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common
import Data.Map
import Data.List (elemIndex,findIndices)
import System.Random
import Text.JSON
import Text.JSON.Generic

import InnerEar.Widgets.SpecEval
import InnerEar.Types.Data
import InnerEar.Types.Score
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Types.Exercise
import InnerEar.Types.ExerciseId
import InnerEar.Types.Frequency
import InnerEar.Exercises.MultipleChoice


prototypeExercise :: MonadWidget t m => Exercise t m WhatBandsAreAllowed [Frequency] Frequency (Map Frequency Score)
prototypeExercise = multipleChoiceExercise
  frequencies
  playFrequency
  PrototypeExercise
  AllBands
  prototypeConfigWidget
  (displayCurrentSpectrumEvaluation (constDyn "Session Performance"))
  prototypeGenerateQuestion
  (Just "Please write some brief text reflecting on your experience:")


frequencies :: [Frequency]
frequencies = [
  F 31 "31", F 63 "63", F 125 "125", F 250 "250", F 500 "500",
  F 1000 "1k", F 2000 "2k", F 4000 "4k", F 8000 "8k", F 16000 "16k"]


playFrequency :: WhatBandsAreAllowed -> Frequency -> Sound
playFrequency _ f = FilteredSound source filter
  where source = NodeSource (BufferNode $ File "pinknoise.wav") 2.0
        filter = Filter Peaking (freqAsDouble f) 1.4 16.0


data WhatBandsAreAllowed = AllBands | HighBands | MidBands | Mid8Bands | LowBands deriving (Show,Eq,Data,Typeable)


prototypeConfigWidget :: MonadWidget t m => WhatBandsAreAllowed -> m (Event t WhatBandsAreAllowed)
prototypeConfigWidget i = do
  let radioButtonMap =  zip [0::Int,1..] [AllBands,HighBands,MidBands,Mid8Bands,LowBands]
  let iVal = maybe 0 id $ elemIndex i [AllBands,HighBands,MidBands,Mid8Bands,LowBands]
  elClass "div" "configText" $ text "Please choose the spectrum range you would like to practice:"
  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ fmap (\(x,y)->(x,show y)) radioButtonMap)
           (WidgetConfig {_widgetConfig_initialValue= Just iVal
                         ,_widgetConfig_setValue = never
                         ,_widgetConfig_attributes = constDyn empty})
  dynConfig <- holdDyn AllBands $ fmap (\x-> maybe AllBands id $ Data.Map.lookup (maybe 0 id x) (fromList radioButtonMap)) (_hwidget_change radioWidget)
  b <- button "Continue to Exercise"
  return $ tagDyn dynConfig b


convertBands :: WhatBandsAreAllowed -> [Bool]
convertBands AllBands = replicate 10 True
convertBands HighBands = [False,False,False,False,False,True,True,True,True,True]
convertBands MidBands = [False,False,False,True,True,True,True,True,False,False]
convertBands Mid8Bands = [False,True,True,True,True,True,True,True,True,False]
convertBands LowBands = [True,True,True,True,True,False,False,False,False,False]


prototypeGenerateQuestion :: WhatBandsAreAllowed -> [Datum WhatBandsAreAllowed [Frequency] Frequency (Map Frequency Score)] -> IO ([Frequency],Frequency)
prototypeGenerateQuestion config prevData = do
  let config' = convertBands config
  let x = fmap (\i-> frequencies!!i) $ findIndices (==True) config'
  y <- getStdRandom ((randomR (0,(length x) - 1))::StdGen -> (Int,StdGen))
  return (x,x!!y)
