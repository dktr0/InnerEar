{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.AddedWhiteNoise (addedWhiteNoiseExercise) where

import Reflex
import Reflex.Dom
import Data.Map
import Text.JSON
import Text.JSON.Generic

import Reflex.Synth.Types
import InnerEar.Exercises.MultipleChoice
import InnerEar.Types.ExerciseId
import InnerEar.Types.Exercise
import InnerEar.Types.Score
import InnerEar.Widgets.Config
import InnerEar.Widgets.SpecEval
import InnerEar.Types.Data (Datum)

type Config = Double -- representing level of attenuation for added white noise

configs :: [Config]
configs = [-10,-20,-30,-40,-50,-60,-65,-70,-75,-80]

configMap:: Map String Config
configMap = fromList $ fmap (\x-> (show x++" dB", x)) configs

data Answer = Answer Bool deriving (Eq,Ord,Data,Typeable)

instance Show Answer where
  show (Answer True) = "With Noise"
  show (Answer False) = "Clean"

answers = [Answer False,Answer True]

renderAnswer :: Config -> b -> Maybe Answer -> Sound
renderAnswer db b (Just (Answer True)) = NoSound -- should be soundSource (b) at -10 plus whiteNoise at dB
renderAnswer db b (Just (Answer False)) = NoSound -- should just be soundSource (b) at -10
renderAnswer db _ Nothing = NoSound -- should also just be soundSource (b) at -10
-- note also: default sound source for this is a 300 Hz sine wave, but user sound files are possible
-- pink or white noise should NOT be possible as selectable sound source types

configurationWidget :: MonadWidget t m => Config -> m (Event t Config)
configurationWidget i = radioConfigWidget "" msg configs i
  where msg = "Please choose the level of (potential) added white noise for this exercise:"

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers

generateQ :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion [Answer False,Answer True]

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text ""
  elClass "div" "instructionsText" $ text ""

addedWhiteNoiseExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
addedWhiteNoiseExercise = multipleChoiceExercise
  1
  [Answer False,Answer True]
  instructions
  (sineSourceConfig "addedWhiteNoiseExercise" configMap)
  renderAnswer
  AddedWhiteNoise
  (-10)
  configurationWidget
  displayEval
  generateQ
