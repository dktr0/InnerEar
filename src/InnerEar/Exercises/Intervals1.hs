{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.Intervals1 (intervals1Exercise) where

import Reflex
import Reflex.Dom
import Sound.MusicW

import Data.Map

import Text.JSON
import Text.JSON.Generic

import InnerEar.Exercises.MultipleChoice
import InnerEar.Types.ExerciseId
import InnerEar.Types.Exercise
import InnerEar.Types.Score
import InnerEar.Types.MultipleChoiceStore
import InnerEar.Types.Data hiding (Time)
import InnerEar.Types.Utility
import InnerEar.Widgets.SpecEval
import InnerEar.Widgets.Config
import InnerEar.Widgets.AnswerButton

type Config = ()

data Answer = P1 | M2 | M3 | P4 | P5
  deriving (Eq,Ord,Data,Typeable)

answerToSemitones :: Answer -> Double
answerToSemitones P1 = 0.0
answerToSemitones M2 = 2.0
answerToSemitones M3 = 4.0
answerToSemitones P4 = 5.0
answerToSemitones P5 = 7.0

instance Show Answer where
  show P1 = "Perfect Unison"
  show M2 = "Major 2nd"
  show M3 = "Major 3rd"
  show P4 = "Perfect 4th"
  show P5 = "Perfect 5th"

instance Buttonable Answer where
  makeButton = showAnswerButton

answers = [P1,M2,M3,P4,P5]

baseTone :: Frequency
baseTone = Midi 60

-- *** note: random pitches requires renderAnswer to return IO Sound instead of Sound
--     ^ then the pitch generation belongs in the Config where the config widget can perform the IO

renderAnswer :: Map String AudioBuffer -> Config -> (SourceNodeSpec, Maybe Time) -> Maybe Answer -> Synth ()
renderAnswer _ _ _ Nothing = buildSynth_ $ silent >> destination
renderAnswer _ _ _ (Just interval) = buildSynth_ $ do
  osc <- oscillator Triangle baseTone
  let amp = Db $ fromIntegral (-20)
  g <- rectEnv (Millis 100) (Sec 1) amp
  let firstDur = (Millis $ 2 * 100) + (Sec 1)
  let rest = Sec 0.5
  -- Change the frequency of the oscillator after the first playback.
  setParamValue "frequency" (inHz $ Midi $ answerToSemitones interval + inMidi baseTone) firstDur osc
  -- Reset for second note and have another rectEnv at firstDur.
  setParamValue "gain" 0 (firstDur + rest) g
  linearRampToParamValue "gain" (inAmp amp) (firstDur + rest + Millis 100) g
  setParamValue "gain" (inAmp amp) (firstDur + rest + Millis 100 + Sec 1) g
  linearRampToParamValue "gain" 0 (firstDur + rest + Millis (2 * 100) + Sec 1) g
  destination
  setDeletionTime (firstDur*2+rest +(Sec 0.5))

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text "Instructions placeholder"


displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> Dynamic t (MultipleChoiceStore Config Answer) -> m ()
displayEval e _ = displayMultipleChoiceEvaluationGraph ("scoreBarWrapperFiveBars","svgBarContainerFiveBars","svgFaintedLineFiveBars", "xLabelFiveBars") "Session performance" "Hz" answers e

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion answers

intervals1Exercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score) (MultipleChoiceStore Config Answer)
intervals1Exercise = multipleChoiceExercise
  3
  answers
  instructions
  (\_ _-> return (constDyn (), constDyn (Just (Silent, Nothing)), never, never))
  renderAnswer
  Intervals1
  ()
  (\_ _ -> return ())
  generateQ
  (const (0,2))
