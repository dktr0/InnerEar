{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module InnerEar.Exercises.RT60 (rt60Exercise) where

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

type Config = () -- exercise is not configurable

data Answer = -- possible answers are five comparable reverbs with distinct RT60 times
  Reverb1 |
  Reverb2 |
  Reverb3 |
  Reverb4 |
  Reverb5 deriving (Eq,Ord,Data,Typeable)

instance Show Answer where
  show Reverb1 = "Reverb1" -- note: should be description of RT60 characteristic instead
  show Reverb2 = "Reverb2"
  show Reverb3 = "Reverb3"
  show Reverb4 = "Reverb4"
  show Reverb5 = "Reverb5"

answers = [Reverb1,Reverb2,Reverb3,Reverb4,Reverb5]

renderAnswer :: Config -> b -> Maybe Answer -> Sound
renderAnswer _ b (Just Reverb1) = NoSound -- should be soundSource (b) at -10 passed through reverb
renderAnswer _ b (Just Reverb2) = NoSound
renderAnswer _ b (Just Reverb3) = NoSound
renderAnswer _ b (Just Reverb4) = NoSound
renderAnswer _ b (Just Reverb5) = NoSound
renderAnswer _ _ Nothing = NoSound -- should also just be soundSource (b) at -10 (dry)
-- note also: default sound source for this is a single sample impulse
-- _short_ pink or white noise bursts might be okay as well, as are user provided sounds

configurationWidget :: MonadWidget t m => Config -> m (Event t Config)
configurationWidget i = do
  x <- getPostBuild
  return $ (() <$ x)

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers

generateQ :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion answers

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text ""
  elClass "div" "instructionsText" $ text ""

configPlaceholder :: forall t m. MonadWidget t m => Config -> m (Dynamic t Config, Dynamic t Source, Event t (Maybe Answer))
configPlaceholder _ = do
  let config = constDyn () :: Dynamic t ()
  let source = constDyn $ NodeSource SilentNode (Just 2.0) :: Dynamic t Source
  return (config,source,never)

rt60Exercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
rt60Exercise = multipleChoiceExercise
  3
  answers
  instructions
  configPlaceholder
  renderAnswer
  RT60
  ()
  configurationWidget
  displayEval
  generateQ
