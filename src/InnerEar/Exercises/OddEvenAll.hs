{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.OddEvenAll (oddEvenAllExercise) where

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

type Config = Frequency -- represents fundamental frequency for sound generation

configs :: [Config]
configs = [F 100 "100 Hz",F 200 "200 Hz", F 400 "400 Hz", F 800 "800 Hz", F 1600 "1600Hz", F 3200 "3200Hz"]

data Answer = Odd | Even | All deriving (Eq,Ord,Data,Typeable,Show)

answers = [Odd,Even,All]

renderAnswer :: Config -> Source -> Maybe Answer -> Sound

renderAnswer f0 _ (Just Odd) = GainSound (OverlappedSound "arbitrary" $ bunchOfOscillators) (-20)
  where
    fs = Prelude.filter (< 20000) $ take 200 $ fmap (* f0) [1,3 .. ] -- :: [Frequency]
    gs = [0,(-6) .. ]
    bunchOfOscillators = fmap (\(x,y) -> Sound $ NodeSource (OscillatorNode $ Oscillator Sine (freqAsDouble x) y) (Just 2.0)) $ zip fs gs

renderAnswer f0 _ (Just Even) = GainSound (OverlappedSound "arbitrary" $ bunchOfOscillators) (-20)
  where
    fs = Prelude.filter (< 20000) $ take 200 $ fmap (* f0) [2,4 .. ] -- :: [Frequency]
    gs = [0,(-6) .. ]
    bunchOfOscillators = fmap (\(x,y) -> Sound $ NodeSource (OscillatorNode $ Oscillator Sine (freqAsDouble x) y) (Just 2.0)) $ zip fs gs

renderAnswer f0 _ (Just All) = GainSound (OverlappedSound "arbitrary" $ bunchOfOscillators) (-20)
  where
    fs = Prelude.filter (< 20000) $ take 200 $ fmap (* f0) [1,2 .. ] -- :: [Frequency]
    gs = [0,(-6) .. ]
    bunchOfOscillators = fmap (\(x,y) -> Sound $ NodeSource (OscillatorNode $ Oscillator Sine (freqAsDouble x) y) (Just 2.0)) $ zip fs gs

renderAnswer f0 _ Nothing = NoSound

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval _ = return ()

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion answers

oddEvenAllConfigWidget:: MonadWidget t m => Config -> m (Dynamic t Config, Dynamic t Source, Event t (Maybe a))
oddEvenAllConfigWidget c = do
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

oddEvenAllExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
oddEvenAllExercise = multipleChoiceExercise
  1
  answers
  instructions
  oddEvenAllConfigWidget
  renderAnswer
  OddEvenAll
  (-10)
  displayEval
  generateQ
