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
import InnerEar.Types.Data

type Config = Double -- representing level of attenuation for added white noise

configs :: [Config]
configs = [-10,-20,-30,-40,-50,-60,-65,-70,-75,-80]

configMap:: Map Int (String,Config)
configMap = fromList $ zip [0::Int,1..] $ fmap (\x-> (show x++" dB", x)) configs

data Answer = Answer Bool deriving (Eq,Ord,Data,Typeable)

instance Show Answer where
  show (Answer True) = "With Noise"
  show (Answer False) = "Clean"

answers = [Answer False,Answer True]

-- Nothing -> no effect (reference)
-- Just True -> with added noise
-- Just Fals -> no added noise

renderAnswer :: Config -> Source -> Maybe Answer -> Sound
renderAnswer db (NodeSource (BufferNode (LoadedFile s p)) Nothing) (Just (Answer True)) = OverlappedSound "addedWhiteNoiseExercise"  [GainSound (Sound $ NodeSource (BufferNode  (LoadedFile s p)) Nothing) (-10) , GainSound (Sound $ NodeSource (BufferNode $ File "whitenoise.wav") Nothing) db] (if (loop p) then Max else OnBufferEnd s)
renderAnswer db (NodeSource node dur) (Just (Answer True)) = OverlappedSound "addedWhiteNoiseExercise"  [GainSound (Sound $ NodeSource node dur) (-10) , GainSound (Sound $ NodeSource (BufferNode $ File "whitenoise.wav") dur ) db] Min  -- should be soundSource (b) at -10 plus whiteNoise at dB
renderAnswer db b (Just (Answer False)) = OverlappedSound "addedWhiteNoiseExercise" [GainSound (Sound b) (-10)] Min -- note: this must be an overlapped sound so that it cuts off the previous playing sound...
renderAnswer db b Nothing = OverlappedSound "addedWhiteNoiseExercise" [GainSound (Sound b) (-10)] Min-- should also just be soundSource (b) at -10
-- note also: default sound source for this is a 300 Hz sine wave, but user sound files are possible
-- pink or white noise should NOT be possible as selectable sound source types



-- data Source = BufferSource JSVal (Maybe Time)| SineWave String Double Time
-- Duraton of buffer can't be decided from JSVal without IO - should Source (or whatever type it is) have something to specify duration?
-- Only a Buffer that is looping will have indefinite duration -
-- Need some sort of duration parameter here so that 'setDeletionTime' is called properly, the right envelope is applied
--should that 'Time' value shouldn't be part of the 'Source' type (or SourceNodeSpec)?
  -- One reason for no: in audio buffer source nodes where duration must be infered by the buffer length, any Instatiation of the Spec would be 'dirtied' by the IO monad because need to perform an IO operation to get the duration of an audio buffer
-- is bMap really so fundamental to need to be part of render answer? only really need it for this exercise?

-------------- To be put somewhere else ...
createSrc:: SourceNodeSpec -> SynthBuilder Graph
createSrc (SoundFile buf pbp) = audioBufferSource $ Buffer buf pbp
createSrc (Oscillator t f ) = oscillator t f

-- ...


g:: Maybe Time -> Amplitude -> SynthBuilder Graph
g dur amp= maybe (gain amp) (\d-> do
  asr (Sec 0.001) d (Sec 0.001) amp) dur


maybeDelete:: Maybe Time -> SynthBuilder () -- move to where setDeletionTime
maybeDelete = maybe (return ()) (setDeletionTime)

renderAnswer::Map String Buffer -> Config -> Source -> Maybe Answer -> Synth ()

getPath WhiteNoise
".reflexsynth/whitenoise.wav"
"Symbol("somethin")" - js symbol?

-- Use this.
renderAnswer:: Config -> (SourceNodeSpec,Maybe Time)-> Maybe Answer -> Synth ()

------------- render answer
renderAnswer::Map String Buffer -> Config -> (SourceNodeSpec,Maybe Time)-> Maybe Answer -> Synth ()
renderAnswer bMap db (src, dur) (Just (Answer True)) = buildSynth $ do
  let env = maybeSynth (rectEnv (Millis 1)) dur
  synthSource src >> gain (Db $ -10) >> env >> destination
  synthSource (Buffer (bMap!!"whitenoise") (PlaybackParam 0 1 (isJust dur))) >> env >> destination
  maybeDelete (fmap (+Sec 0.2) dur)
renderAnswer bMap db (src, dur) _ = buildSynth $ createSrc src >> (getEnv env $ Db -10) >> destination


data Buffer  = Buffer JSVal

------------------------------------------------------ This isn't actualy correct, not applyign the config db to the noise...
renderAnswer::Map String Buffer -> Config -> (SourceNodeSpec,Maybe Time)-> Maybe Answer -> Synth ()
renderAnswer bMap db (src, dur) (Just (Answer True)) = buildSynth $ do
  let env = maybe (gain $ Db $ -10) (\d-> do
    asr (Sec 0.01) d (Sec 0.01) (Db $ -10)
    setDeletionTime (d+ Sec 0.2)
    ) dur
    createSrc src >> env >> destination
    audioBufferSource (bMap!!"whitenoise") (PlaybackParam 0 1 (isJust dur)) >> env >> destination
renderAnswer bMap db (src, dur) _ = buildSynth $ do
      let env = maybe (gain $ Db $ -10) (\d-> do
        asr (Sec 0.01) d (Sec 0.01) (Db $ -10)
        setDeletionTime (d+ Sec 0.2)
        ) dur
        createSrc src >> env >> destination


----------------------------------------------------------

  if isJust dur then (asr (Sec 0.01) duration (Sec 0.01) (Db $ -10)) else (gain (Db $ -10))
  createSrc src >> env >>

  if isJust dur then
    do
      let duration = fromJust dur
      createSrc src >> asr (Sec 0.01) duration (Sec 0.01) (Db $ -10) >> destination

      audioBufferSource (bMap!!"whitenoise") (PlaybackParam 0 1 True)
      asr (Sec 0.01) dur (Sec 0.01) (Db $ -10) >>
      setDeletionTime (dur+)
    else do
      createSr

let f x = do; a <-func; return a; where func = if x then (getLine) else (putStrLn "testing" >> return "testing") ;

renderAnswer::Map String Buffer -> Config -> SourceNodeSpec-> Maybe Answer -> Synth ()
renderAnswer bMap db (Buffer x p) (Just (Answer True)) = buildSynth $ do
  audioBufferSource x p >> gain (Db (-10)) >> destination
  audioBufferSource (bMap!!"whitenoise")
  g<-gain (Db db)
  if loop p then return () else do
   setParamValue "gain" 0.0 (duration x) g >> destination
   setDeletionTime (duration x + Sec 0.2)
renderAnswer bMap db (SineWave t freq dur) (Just (Answer True)) = buildSynth $ do
  oscillator t freq >> asr (Sec 0.01) dur (Sec 0.01) (Db -10)) >> destination
  audioBufferSource (bMap!!"whitenoise")
  gain (Db db) >>= setParamValue "gain" 0.0 (dur+0.02) >>  destination
  setDeletionTime (dur+Sec 0.2)
renderAnswer bMap db ()



displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion [Answer False,Answer True]

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text "In this exercise, a low level of noise (white noise) is potentially added to a reference signal. Your task is to detect whether or not the noise has been added. Configure the level of the noise progressively lower and lower to challenge yourself."
  elClass "div" "instructionsText" $ text "Note: the exercise will work right away with a sine wave as a reference tone (to which noise is or is not added), however it is strongly recommended that the exercise be undertaken with recorded material such as produced music, field recordings, etc. Click on the sound source menu to load a sound file from the local filesystem."



sourcesMap:: Map Int (String,Source)
sourcesMap = fromList $ [(0,("300hz sine wave", NodeSource (OscillatorNode $ Oscillator Sine 440 0) (Just 2))), (1,("Load a soundfile", NodeSource (BufferNode $ LoadedFile "addedWhiteNoiseExercise" (PlaybackParam 0 1 False)) Nothing))]

addedWhiteNoiseExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
addedWhiteNoiseExercise = multipleChoiceExercise
  1
  [Answer False,Answer True]
  instructions
  (configWidget "addedWhiteNoiseExercise" sourcesMap 0 "Noise level (dB): " configMap)
  renderAnswer
  AddedWhiteNoise
  (-10)
  displayEval
  generateQ
