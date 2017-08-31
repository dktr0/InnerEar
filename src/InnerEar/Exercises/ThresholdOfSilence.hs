module InnerEar.Exercises.ThresholdOfSilence (thresholdOfSilenceExercise) where

import Reflex
import Reflex.Dom

import InnerEar.Exercises.MultipleChoice
import InnerEar.Types.ExerciseId

type Config = Int -- gain value for attenuated sounds

configs :: [Config]
configs = [-20,-30,-40,-50,-60,-70,-80,-90,-100,-110]

data Answer = Answer Bool deriving (Eq,Ord)

instance Show Answer where
  show (Answer True) = "Attenuated Sound"
  show (Answer False) = "No sound at all"

sound :: Config -> Answer -> Sound () -- ?

sound db (Answer True) = fileSource "pinkNoise.wav" 2.0 >>= applyGain db

sound _ (Answer False) = silentSource 2.0

configWidget :: MonadWidget t m => Int -> m (Event t Int)
configWidget i = do
  let radioButtonMap = (fromList $ zip [0,1..] configs) :: Map Int Int
  elClass "div" "configText" $ text "Please choose the level of attenuation for this exercise:"
  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ toList $ fmap show radioButtonMap)
           (WidgetConfig {_widgetConfig_initialValue = Just $ maybe 0 id $ elemIndex i configs
                         ,_widgetConfig_setValue = never
                         ,_widgetConfig_attributes = constDyn empty})
  dynConfig <- holdDyn (configs!!i) $ fmap (\x-> maybe (configs!!i) id $ Data.Map.lookup (maybe i id x) radioButtonMap) $ _hwidget_change radioWidget)
  button "Continue to Exercise" >>= tagDyn dynConfig

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval scoreMap = do


generateQuestion :: Int -> [Datum Int [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQuestion config _ = do
  x <- getStdRandom ((randomR (0,1)) :: StdGen -> (Int,StdGen))
  let y = fmap (\z -> if z==0 then False else True) x
  return ([False,True],y)


thresholdOfSilenceExercise :: MonadWidget t m => Exercise t m Int [Answer] Answer (Map Answer Score)
thresholdOfSilenceExercise = multipleChoiceExercise
  [Answer False,Answer True]
  sound
  ThresholdOfSilence
  (-20)
  configWidget
  displayEvalWidget
  generateQuestion
  Just "Please write a reflection here..."
