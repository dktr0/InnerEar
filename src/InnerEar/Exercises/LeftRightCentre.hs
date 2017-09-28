{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.LeftRightCentre (leftRightCentreExercise) where

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

type Config = Double -- representing percentage of how far to left or right things are panned
-- i.e. 0.5 = things are only panned half-way to the left or right if they are panned

configs :: [Config]
configs = [1.0,0.5,0.25,0.1,0.05]

configMap:: Map String Config
configMap = fromList $ fmap (\x-> (show (x*100) ++ "%", x)) configs

data Answer =
  PanLeft |
  PanRight |
  PanCentre deriving (Eq,Data,Typeable)

instance Show Answer where
  show PanLeft = "Panned Left"
  show PanCentre = "Centre-panned"
  show PanRight = "Panned Right"

instance Ord Answer where
  PanLeft <= PanCentre = True
  PanLeft <= PanRight = True
  PanCentre <= PanRight = True
  _ <= _ = False

answers = [PanLeft,PanCentre,PanRight]

renderAnswer :: Config -> b -> Maybe Answer -> Sound
renderAnswer p b (Just PanLeft) = NoSound -- should be source b panned left by percentage b (note: 0-1) and down -10 dB
renderAnswer p b (Just PanRight) = NoSound -- should be source b panned right by percentage b (note: 0-1) and down -10 dB
renderAnswer p b (Just PanCentre) = NoSound -- should be source b centre-panned (note: 0-1) and down -10 dB
renderAnswer _ b Nothing = NoSound -- should be the same as Centre answer above
-- note: valid sound sources are noise, sine waves, user-provided files

configurationWidget :: MonadWidget t m => Config -> m (Event t Config)
configurationWidget i = radioConfigWidget "" msg configs i
  where msg = "Please choose the percent to the left or right that panned sounds are panned for this exercise:"

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers

generateQ :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion answers

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text ""
  elClass "div" "instructionsText" $ text ""

leftRightCentreExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
leftRightCentreExercise = multipleChoiceExercise
  1
  answers
  instructions
  (sineSourceConfig "leftRightCentreExercise" configMap)
  renderAnswer
  LeftRightCentre
  (1.0)
  configurationWidget
  displayEval
  generateQ
