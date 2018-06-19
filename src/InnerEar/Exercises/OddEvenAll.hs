{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.OddEvenAll (oddEvenAllExercise) where

import Reflex
import Reflex.Dom
import Data.Map
import Text.JSON
import Text.JSON.Generic


import Sound.MusicW hiding (Frequency)
import InnerEar.Exercises.MultipleChoice
import InnerEar.Types.ExerciseId
import InnerEar.Types.Exercise
import InnerEar.Types.Score
import InnerEar.Types.MultipleChoiceStore
import InnerEar.Widgets.Config
import InnerEar.Widgets.SpecEval

import InnerEar.Types.Data hiding (Time)


import InnerEar.Types.Frequency
import InnerEar.Widgets.AnswerButton

type Config = Frequency -- represents fundamental frequency for sound generation

configs :: [Config]
configs = [F 100 "100 Hz",F 200 "200 Hz", F 400 "400 Hz", F 800 "800 Hz", F 1600 "1600Hz", F 3200 "3200Hz"]

data Answer = Odd | Even | All deriving (Eq,Ord,Data,Typeable,Show)

instance Buttonable Answer where
  makeButton = showAnswerButton

answers = [Odd,Even,All]

renderAnswer :: Map String AudioBuffer -> Config -> (SourceNodeSpec,Maybe Time) -> Maybe Answer -> Synth ()
renderAnswer _ f0 _ (Just a) = buildSynth $ do
  let env = asr (Sec 0.01) (Sec 2) (Sec 0.01) (Amp 1)
  let masterGain = gain (Db $ -10)
  mapM_ (\(f,g) -> oscillator Sine f >> gain g >> env >> masterGain >> destination) oscSpecs
  setDeletionTime (Sec 2.5)
  where
    fs = fmap Hz $ Prelude.filter (< 20000) $ take 200 $ fmap (* (freqAsDouble f0)) $ case a of
      Odd -> [1,3 .. ]
      Even -> (1:[2,4..])
      All -> [1,2 .. ]
    gs = fmap Db [0,(-6) .. ]
    oscSpecs = zip fs gs
renderAnswer _ f0 _ Nothing = return ()

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> Dynamic t (MultipleChoiceStore Config Answer) -> m ()
displayEval e _ = displayMultipleChoiceEvaluationGraph  ("scoreBarWrapperThreeBars","svgBarContainerThreeBars","svgFaintedLineThreeBars","xLabelThreeBars") "Session Performance" "" answers e

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion answers

oddEvenAllConfigWidget :: MonadWidget t m => Map String AudioBuffer -> Config -> m (Dynamic t Config, Dynamic t (Maybe (SourceNodeSpec, Maybe Time)), Event t (), Event t ())
oddEvenAllConfigWidget _ c = do
  text "Fundamental Frequency: "
  dd <- dropdown (freqAsDouble $ head configs) (constDyn $ fromList $ fmap (\x-> (freqAsDouble x, freqAsString x)) configs) (DropdownConfig never (constDyn empty))
  let ddVal = _dropdown_value dd -- Dynamic Double
  conf <- mapDyn (\x -> F x (show x++" Hz")) ddVal
  source <- mapDyn (\x -> Just (Oscillator Sine (Hz x), Just (Sec 2))) ddVal
  return (conf, source, never, never)

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text "Instructions placeholder"

oddEvenAllExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score) (MultipleChoiceStore Config Answer)
oddEvenAllExercise = multipleChoiceExercise
  2
  answers
  instructions
  oddEvenAllConfigWidget
  renderAnswer
  OddEvenAll
  (-10)
  displayEval
  generateQ
  (const (0,2))
