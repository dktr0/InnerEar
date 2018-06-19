{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.FrequencyEnvelope (frequencyEnvelopeExercise) where

import Reflex
import Reflex.Dom
import Data.Map
import Text.JSON
import Text.JSON.Generic

import Reflex.Synth.Synth
import InnerEar.Exercises.MultipleChoice
import InnerEar.Types.ExerciseId
import InnerEar.Types.Exercise
import InnerEar.Types.Score
import InnerEar.Widgets.Config
import InnerEar.Widgets.SpecEval
import InnerEar.Types.Data hiding (Time)

import InnerEar.Types.Frequency
import InnerEar.Types.Utility
import InnerEar.Widgets.AnswerButton

type Similarity = Int
type Duration = Int
type Octave = Double
type Config = (Similarity,Duration,Octave)

similarities :: [Similarity]
similarities = [1,2,3,4]

durations :: [Duration]
durations = [1000,500,250,125,100,80,60,40,20]

octaves :: [Octave]
octaves = [8.0,4.0,2.0,1.0,0.5,1/6,1/12]

data Answer = Quicker | Linear | Slower
  deriving (Eq,Ord,Data,Typeable,Show)

instance Buttonable Answer where
  makeButton = showAnswerButton

answers = [Quicker,Linear,Slower]

getEnvelope :: Similarity -> Answer -> (Double -> Double)
getEnvelope s Quicker = riseToRiseAndFall $ quickerEnvelope (similarityToExp s)
getEnvelope s Linear = riseToRiseAndFall $ linearEnvelope (similarityToExp s)
getEnvelope s Slower = riseToRiseAndFall $ slowerEnvelope (similarityToExp s)

similarityToExp :: Int -> Double
similarityToExp 1 = 3
similarityToExp 2 = 2
similarityToExp 3 = 1.5
similarityToExp 4 = 1.25

slowerEnvelope :: Double -> Double -> Double
slowerEnvelope e x = x**e

linearEnvelope :: Double -> Double -> Double
linearEnvelope _ x = x

quickerEnvelope :: Double -> Double -> Double
quickerEnvelope e x = 1 - (slowerEnvelope e (1-x))

lowPitch :: Octave -> Double -- double is Frequency
lowPitch o = midicps $ 69-(o*12/2)

highPitch :: Octave -> Double -- double is Frequency
highPitch o = midicps $ 69+(o*12/2)

actualEnvelope :: Config -> Answer -> Double -> Double
actualEnvelope (s,d,o) a = scaleRange (lowPitch o) (highPitch o) $ scaleDomain 0.0 (fromIntegral(d)/1000.0) $ getEnvelope s a

sampleEnvelope :: Int -> (Double -> Double) -> Duration -> [Double]
sampleEnvelope r f d = fmap (f . (\x -> x * fromIntegral(d)/1000.0/fromIntegral(r))) $ fmap fromIntegral [0, 1 .. (r-1)]

renderAnswer :: Map String AudioBuffer -> Config -> (SourceNodeSpec, Maybe Time)-> Maybe Answer -> Synth ()
renderAnswer _ c@(s, d, o) _ (Just ans) = buildSynth_ $ do
  let dur = Millis $ fromIntegral d
  let curve = sampleEnvelope 200 (actualEnvelope c ans) d
  let env = unitRectEnv (Millis 1) dur --maybe (return EmptyGraph) (unitRectEnv (Millis 1)) dur
  setDeletionTime $ dur + Millis 200
  oscillator Triangle (Hz 0) >>= curveToParamValue "frequency" curve (Sec 0) dur
  gain (Db $ fromIntegral $ -20)
  env
  destination
renderAnswer _ _ _ Nothing = buildSynth_ $ silent >> destination

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval _ = return ()

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion answers

thisConfigWidget:: MonadWidget t m => Map String AudioBuffer -> Config -> m (Dynamic t Config, Dynamic t (Maybe (SourceNodeSpec, Maybe Time)), Event t (), Event t ())
thisConfigWidget _ c@(s,d,o) = do
  text "Similarity: "
  simDropDown <- dropdown (head similarities) (constDyn $ fromList [ (x,show x) | x <- similarities ]) (DropdownConfig never (constDyn empty))
  text "Duration: "
  durDropDown <- dropdown (head durations) (constDyn $ fromList [ (x,show x) | x <- durations]) (DropdownConfig never (constDyn empty))
  text "Octaves: "
  octDropDown <- dropdown (head octaves) (constDyn $ fromList [ (x,show x) | x <- octaves]) (DropdownConfig never (constDyn empty))
  let sim = _dropdown_value simDropDown
  let dur = _dropdown_value durDropDown
  let oct = _dropdown_value octDropDown
  simDur <- combineDyn (,) sim dur
  simDurOct <- combineDyn (,) simDur oct
  conf <- mapDyn (\((x,y),z) -> (x,y,z)) simDurOct
  let source = constDyn $ Just (Silent, Nothing)
  return (conf, source, never, never)

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text "Instructions placeholder"

frequencyEnvelopeExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
frequencyEnvelopeExercise = multipleChoiceExercise
  2
  answers
  instructions
  thisConfigWidget
  renderAnswer
  FrequencyEnvelope
  (1,1000,8.0)
  displayEval
  generateQ
