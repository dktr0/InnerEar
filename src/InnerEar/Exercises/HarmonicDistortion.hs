{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.HarmonicDistortion (harmonicDistortionExercise) where

import Reflex
import Reflex.Dom

import InnerEar.Exercises.MultipleChoice
import InnerEar.Types.ExerciseId

type Config = Double -- representing threshold of clipping, and inverse of post-clip normalization

configs :: [Config]
configs = [-12,-6,-3,-2,-1,-0.75,-0.5,-0.25,-0.125,-0.0625]

data Answer = Answer Bool deriving (Eq,Ord)

instance Show Answer where
  show (Answer True) = "Distorted"
  show (Answer False) = "Clean"

sound :: Config -> Answer -> Sound
sound db (Answer True) = NoSound 2.0 -- should be a sine wave clipped and normalized by db, then attenuated a standard amount (-10 dB)
sound db (Answer False) = NoSound 2.0 -- should be a clean sine wave, just attenuated a standard amount (-10 dB)

configWidget :: MonadWidget t m => Config -> m (Event t Config)
configWidget i = do
  let radioButtonMap = (fromList $ zip [0,1..] configs) :: Map Int Config
  elClass "div" "configText" $ text "Please choose the level of clipping for this exercise:"
  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ toList $ fmap show radioButtonMap)
           (WidgetConfig {_widgetConfig_initialValue = Just $ maybe 0 id $ elemIndex i configs
                         ,_widgetConfig_setValue = never
                         ,_widgetConfig_attributes = constDyn empty})
  dynConfig <- holdDyn (configs!!i) $ fmap (\x-> maybe (configs!!i) id $ Data.Map.lookup (maybe i id x) radioButtonMap) $ _hwidget_change radioWidget)
  button "Continue to Exercise" >>= tagDyn dynConfig

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval scoreMap = return ()

generateQuestion :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQuestion _ _ = randomMultipleChoiceQuestion [Answer False,Answer True]

harmonicDistortionExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
harmonicDistortionExercise = multipleChoiceExercise
  [Answer False,Answer True]
  sound
  HarmonicDistortion
  (-12)
  configWidget
  displayEval
  generateQuestion
  Just "Please write a reflection here..."
