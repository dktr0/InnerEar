{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.SpectralShape (spectralShapeExercise) where

import Reflex
import Reflex.Dom

import Sound.MusicW hiding(Frequency)
import Data.Map

import Text.JSON
import Text.JSON.Generic

import InnerEar.Exercises.MultipleChoice
import InnerEar.Types.ExerciseId
import InnerEar.Types.Exercise
import InnerEar.Types.Score
import InnerEar.Types.MultipleChoiceStore
import InnerEar.Types.Data hiding (Time)
import InnerEar.Types.Frequency
import InnerEar.Types.Utility
import InnerEar.Widgets.SpecEval
import InnerEar.Widgets.Config
import InnerEar.Widgets.SpecGraph
import InnerEar.Widgets.Lines
import InnerEar.Widgets.AnswerButton
import InnerEar.Widgets.Utility


type Config = Frequency -- represents fundamental frequency for sound generation

configs :: [Config]
configs = [F 100 "100 Hz",F 200 "200 Hz", F 400 "400 Hz", F 800 "800 Hz", F 1600 "1600Hz", F 3200 "3200Hz"]

data Answer = Steep | Linear | Gradual | Flat | InverseGradual | InverseLinear | InverseSteep
  deriving (Eq,Ord,Data,Typeable,Show)

instance Buttonable Answer where
  -- makeButton a m = answerButton' m a $ do
  makeButton a m = clickableDiv (constDyn "someClass") a $ do
    elClass "div" "someClass" $ text (show a)
    elClass "div" "someClass" $ text (show a)
    let xAndYPoints = [ (x*4,y*100) | x <- [1,2 .. 25] , y <- take 25 (getShape a) ]
    elClass "div" "someClass" $ shapeLine' "polyline" xAndYPoints
    return ()

answers = [Steep,Linear,Gradual,Flat,InverseGradual,InverseLinear,InverseSteep]

getShape :: Answer -> [Double]
getShape Steep = fmap (ampdb . (\x -> 1/(x*x))) [1,2 .. 200]
getShape Linear = fmap (ampdb . (\x -> 1/x)) [1,2 .. 200]
getShape Gradual = fmap (ampdb . (\x -> 1/sqrt x)) [1,2 .. 200]
getShape Flat = fmap (ampdb . (\x -> 1)) [1,2 .. 200]
getShape InverseGradual = fmap (ampdb . (\x -> 1/sqrt x)) [1,2 .. 200]
getShape InverseLinear = fmap (ampdb . (\x -> 1/x)) [1,2 .. 200]
getShape InverseSteep = fmap (ampdb . (\x -> 1/(x*x))) [1,2 .. 200]


renderAnswer :: Map String AudioBuffer -> Config -> (SourceNodeSpec,Maybe Time) -> Maybe Answer -> Synth ()
renderAnswer _ f0 _ (Just a) = buildSynth $ do
  let env = asr (Sec 0.01) (Sec 2) (Sec 0.01) (Amp 1)
  let masterGain = gain (Db $ fromIntegral $ -10)
  mapM_ (\(f,g) -> oscillator Sine f >> gain g >> env >> masterGain >> destination) oscSpecs
  setDeletionTime (Sec 2.5)
  where
    fs = fmap Hz $ Prelude.filter (< 20000) $ take 200 $ fmap (* (freqAsDouble f0)) [1,2 .. ]
    gs = fmap Db $ getShape a
    oscSpecs = zip fs gs
renderAnswer _ f0 _ Nothing = return ()


displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> Dynamic t (MultipleChoiceStore Config Answer) -> m ()
displayEval e _ = displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers e

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion answers

thisConfigWidget :: MonadWidget t m => Map String AudioBuffer -> Config -> m (Dynamic t Config, Dynamic t (Maybe (SourceNodeSpec, Maybe Time)), Event t (), Event t ())
thisConfigWidget _ c = do
  text "Fundamental Frequency: "
  dd <- dropdown (freqAsDouble $ head configs) (constDyn $ fromList $ fmap (\x-> (freqAsDouble x, freqAsString x)) configs) (DropdownConfig never (constDyn empty))
  let ddVal = _dropdown_value dd -- Dynamic Double
  conf <- mapDyn (\x -> F x (show x++" Hz")) ddVal
  source <- mapDyn (\x -> Just (Oscillator Sine (Hz x), (Just $ Sec 2))) ddVal
  return (conf, source, never, never)

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text "Instructions placeholder"
  --shapeLine (constDyn "polyline") [(10,10), (20,20), (30,30), (40,40), (50,50), (60,60)]
  graphGen xPoints linearGraphYPoints

spectralShapeExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score) (MultipleChoiceStore Config Answer)
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
  (const (0,2))
