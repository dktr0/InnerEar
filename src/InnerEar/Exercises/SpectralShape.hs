{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.SpectralShape (spectralShapeExercise) where

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
import InnerEar.Types.Frequency
import InnerEar.Types.Utility
import InnerEar.Widgets.SpecGraph
import InnerEar.Widgets.Lines


type Config = Frequency -- represents fundamental frequency for sound generation

configs :: [Config]
configs = [F 100 "100 Hz",F 200 "200 Hz", F 400 "400 Hz", F 800 "800 Hz", F 1600 "1600Hz", F 3200 "3200Hz"]

data Answer = Steep | Linear | Gradual | Flat | InverseGradual | InverseLinear | InverseSteep
  deriving (Eq,Ord,Data,Typeable,Show)

answers = [Steep,Linear,Gradual,Flat,InverseGradual,InverseLinear,InverseSteep]

getShape :: Answer -> [Double]
getShape Steep = fmap (ampdb . (\x -> 1/(x*x))) [1,2 ..]
getShape Linear = fmap (ampdb . (\x -> 1/x)) [1,2 ..]
getShape Gradual = fmap (ampdb . (\x -> 1/sqrt x)) [1,2 ..]
getShape Flat = fmap (ampdb . (\x -> 1)) [1,2 ..]
getShape InverseGradual = fmap (ampdb . (\x -> 1/sqrt x)) [1,2 ..]
getShape InverseLinear = fmap (ampdb . (\x -> 1/x)) [1,2 ..]
getShape InverseSteep = fmap (ampdb . (\x -> 1/(x*x))) [1,2 ..]


renderAnswer :: Config -> Source -> Maybe Answer -> Sound

renderAnswer f0 _ (Just Steep) = GainSound (OverlappedSound "arbitrary" $ bunchOfOscillators) (-5)
  where
    fs = Prelude.filter (< 20000) $ take 200 $ fmap (* f0) [1,2 .. ] -- :: [Frequency]
    gs = getShape Steep
    bunchOfOscillators = fmap (\(x,y) -> Sound $ NodeSource (OscillatorNode $ Oscillator Sine (freqAsDouble x) y) (Just 2.0)) $ zip fs gs

renderAnswer f0 _ (Just Linear) = GainSound (OverlappedSound "arbitrary" $ bunchOfOscillators) (-25)
  where
    fs = Prelude.filter (< 20000) $ take 200 $ fmap (* f0) [1,2 .. ] -- :: [Frequency]
    gs = getShape Linear
    bunchOfOscillators = fmap (\(x,y) -> Sound $ NodeSource (OscillatorNode $ Oscillator Sine (freqAsDouble x) y) (Just 2.0)) $ zip fs gs

renderAnswer f0 _ (Just Gradual) = GainSound (OverlappedSound "arbitrary" $ bunchOfOscillators) (-30)
  where
    fs = Prelude.filter (< 20000) $ take 200 $ fmap (* f0) [1,2 .. ] -- :: [Frequency]
    gs = getShape Gradual
    bunchOfOscillators = fmap (\(x,y) -> Sound $ NodeSource (OscillatorNode $ Oscillator Sine (freqAsDouble x) y) (Just 2.0)) $ zip fs gs

renderAnswer f0 _ (Just Flat) = GainSound (OverlappedSound "arbitrary" $ bunchOfOscillators) (-50)
  where
    fs = Prelude.filter (< 20000) $ take 200 $ fmap (* f0) [1,2 .. ] -- :: [Frequency]
    gs = getShape Flat
    bunchOfOscillators = fmap (\(x,y) -> Sound $ NodeSource (OscillatorNode $ Oscillator Sine (freqAsDouble x) y) (Just 2.0)) $ zip fs gs

renderAnswer f0 _ (Just InverseGradual) = GainSound (OverlappedSound "arbitrary" $ bunchOfOscillators) (-30)
  where
    fs = reverse $ Prelude.filter (< 20000) $ take 200 $ fmap (* f0) [1,2 .. ] -- :: [Frequency]
    gs = getShape InverseGradual
    bunchOfOscillators = fmap (\(x,y) -> Sound $ NodeSource (OscillatorNode $ Oscillator Sine (freqAsDouble x) y) (Just 2.0)) $ zip fs gs

renderAnswer f0 _ (Just InverseLinear) = GainSound (OverlappedSound "arbitrary" $ bunchOfOscillators) (-25)
  where
    fs = reverse $ Prelude.filter (< 20000) $ take 200 $ fmap (* f0) [1,2 .. ] -- :: [Frequency]
    gs = getShape InverseLinear
    bunchOfOscillators = fmap (\(x,y) -> Sound $ NodeSource (OscillatorNode $ Oscillator Sine (freqAsDouble x) y) (Just 2.0)) $ zip fs gs

renderAnswer f0 _ (Just InverseSteep) = GainSound (OverlappedSound "arbitrary" $ bunchOfOscillators) (-5)
  where
    fs = reverse $ Prelude.filter (< 20000) $ take 200 $ fmap (* f0) [1,2 .. ] -- :: [Frequency]
    gs = getShape InverseSteep
    bunchOfOscillators = fmap (\(x,y) -> Sound $ NodeSource (OscillatorNode $ Oscillator Sine (freqAsDouble x) y) (Just 2.0)) $ zip fs gs

renderAnswer f0 _ Nothing = NoSound

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval _ = return ()

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion answers

thisConfigWidget:: MonadWidget t m => Config -> m (Dynamic t Config, Dynamic t Source, Event t (Maybe a))
thisConfigWidget c = do
  text "Fundamental Frequency: "
  dd <- dropdown (freqAsDouble $ head configs) (constDyn $ fromList $ fmap (\x-> (freqAsDouble x, freqAsString x)) configs) (DropdownConfig never (constDyn empty))
  let ddVal = _dropdown_value dd -- Dynamic Double
  conf <- mapDyn (\x -> F x (show x++" Hz")) ddVal
  source <- mapDyn (\x -> NodeSource (OscillatorNode $ Oscillator Sine x (-20)) (Just 2)) ddVal
  -- playRef <- liftM (<$ Nothing) $ button "Play reference sound"
  return (conf, source, never)

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text "Instructions placeholder"
  --shapeLine (constDyn "polyline") [(10,10), (20,20), (30,30), (40,40), (50,50), (60,60)]
  graphGen xPoints linearGraphYPoints

spectralShapeExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
spectralShapeExercise = multipleChoiceExercise
  3
  answers
  instructions
  thisConfigWidget
  renderAnswer
  SpectralShape
  (-10)
  displayEval
  generateQ
