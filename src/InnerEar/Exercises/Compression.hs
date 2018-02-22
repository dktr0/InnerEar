{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.Compression (compressionExercise) where

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
import InnerEar.Types.Data

type Config = Double -- representing compression ratio, i.e. 2 = 2:1 compression ratio

configs :: [Config]
configs = [20,10,5,2,1.5]

configMap:: Map Int (String, Config)
configMap = fromList $ zip [(0::Int),1..] $ fmap (\x-> (show x++":1", x)) configs

data Answer = Answer Bool deriving (Eq,Ord,Data,Typeable)

instance Show Answer where
  show (Answer True) = "Compressed"
  show (Answer False) = "Not Compressed"

answers = [Answer False,Answer True]


renderAnswer :: Config -> Source -> Maybe Answer -> Sound
renderAnswer r b (Just (Answer True)) = GainSound (CompressedSound (Sound b) (Compressor {threshold=(-20),ratio=r,knee=0,attack=0.003,release=0.1})) (-10)
-- should be source (b) compressed at threshold -20 dB with ratio r, then down -10 dB post-compression<
renderAnswer _ b _ = GainSound (Sound b) (-10) -- should just be source (b) down -10 dB
-- note also: the user MUST provide a sound file (or we might provide some standard ones) - synthetic sources won't work for this

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion [Answer False,Answer True]

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text "In this exercise, a reference sound is either compressed or not and your task is to tell whether or not it has been compressed. The threshold of the compressor is set at -20 dBFS, and you can configure the exercise to work with smaller and smaller ratios for increased difficulty. Note that you must provide a source sound to use for the exercise (click on Browse to the right). Short musical excerpts that consistently have strong levels are recommended."


sourcesMap:: Map Int (String,Source)
sourcesMap = singleton 0 ("Load a soundfile", NodeSource (BufferNode $ LoadedFile "compressionExercise" (PlaybackParam 0 1 False)) Nothing)


compressionExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
compressionExercise = multipleChoiceExercise
  1
  answers
  instructions
  (configWidget "compressionExercise" sourcesMap 0 "Compression ratio:  " configMap) -- (dynRadioConfigWidget "fiveBandBoostCutExercise" sourcesMap 0  configMap)
  renderAnswer
  Compression
  (20)
  displayEval
  generateQ
