{-# LANGUAGE RecursiveDo #-}


module InnerEar.Exercises.HarmonicsOne (harmonicsOneExercise) where


import Reflex
import Reflex.Dom
import Data.Map
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common
import System.Random


import InnerEar.Widgets.Utility
import InnerEar.Types.Data
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Widgets.Bars
import InnerEar.Types.Exercise
import InnerEar.Types.ExerciseId
import InnerEar.Types.ExerciseNavigation
import InnerEar.Widgets.Utility

-- c = ()
-- q = ()
-- a = Int
-- e = Int
--[Datum c q a e]

harmonicsOneExercise :: MonadWidget t m => Exercise t m () () Int Int
harmonicsOneExercise = Exercise {
  exerciseId = HarmonicsOne,
  defaultConfig = (),
  defaultEvaluation = 0,
  displayEvaluation = harmoncsOneDisplayEvaluation,
  configWidget = harmonicsOneConfigWidget,
  generateQuestion = harmonicsOneGenerateQuestion,
  questionWidget = harmonicsOneQuestionWidget
}

harmoncsOneDisplayEvaluation _ = return ()

harmonicsOneConfigWidget::MonadWidget t m => () ->  m (Event t ())
harmonicsOneConfigWidget _ = do
  text "Instructions:  Match the sound you hear to the correct harmonic distribution"
  button "go to exercise"


harmonicsOneGenerateQuestion:: () -> [Datum () () Int Int] -> IO ((),Int)
harmonicsOneGenerateQuestion _ _ = do
  x <- getStdRandom ((randomR (0,5))::StdGen -> (Int,StdGen))
  return ((),x)


harmonicsOneQuestionWidget :: MonadWidget t m => () -> Int -> Event t ((),Int) -> m (Event t (Datum () () Int Int),Event t Sound,Event t ExerciseNavigation)
harmonicsOneQuestionWidget _ _ q = mdo
  soundButtons <-el "table" $ do
    soundButtons <- sequence $ mapWithKey (\k v-> el "tr" $ el "td" $ do
      chooseButton <- buttonVal (show k) k
      playButton <- flippableWidget (buttonVal ("play") v) (return never) True (updated isAnswering)
      performSound $ switchPromptlyDyn playButton
      return chooseButton
      ) soundsMap -- m Map Int (Event t Int
    return soundButtons
  soundNum <- holdDyn 0 (fmap snd q)
  sound <- mapDyn (maybe NoSound id . (flip Data.Map.lookup) soundsMap) soundNum
  playButton <- button "play"
  performSound $ tagDyn sound playButton
  userSelection <- holdDyn Nothing $ leftmost $ [fmap (const Nothing) nextButton]++ (fmap (fmap Just) $ elems soundButtons) -- m Map Int (Event t Int
  submitButton <- button "submit"
  userAnswer <- holdDyn Nothing $ tagDyn userSelection submitButton
  answerIsCorrect <- combineDyn (\x y-> maybe False (x==) y) soundNum userAnswer
  isAnswering <- holdDyn False $ leftmost [fmap (const False) submitButton, fmap (const True) nextButton]
  nextButtonWidget <- flippableWidget  (return never) (buttonVal "next" InQuestion) False (leftmost [(True <$) submitButton, (False <$) nextButton])
  let nextButton = switchPromptlyDyn nextButtonWidget
  correctText <- combineDyn (\x y-> if x then "Correct!" else "The correct answer was "++show y) answerIsCorrect soundNum
  flippableWidget (text "") (dynText correctText) False (leftmost [(True <$) submitButton, (False <$) nextButton])
  backToConfigure <- (InConfigure <$) <$> button "Configure"
  onToReflect <- (InReflect <$) <$> button "Reflect"
  return (never ,never ,leftmost [onToReflect,backToConfigure,nextButton])


soundsMap:: Map Int Sound
soundsMap = fromList $ zip [0::Int,1..] sounds
  where
    ex0 = zip (fmap (220*) [1::Double,2..10]) (repeat 1)
    ex1 = zip (fmap (220*) $ take 5 [x*2 | x<-[1..10]]) [1..10]
    ex2 = zip (fmap (220*) [1::Double,2..10]) [1/x | x<-[1..10]]
    ex3 = zip (fmap (220*) $ take 5 [x*2 | x<-[1..10]]) [1/x | x<-[1..10]]
    ex4 = zip (fmap (220*) [x | x<-[1..10]]) (reverse [1/x | x<-[1..10]])
    ex5 = zip (fmap (220*) $ take 5 [x*2 | x<-[1..10]]) (reverse [1/x | x<-[1..10]])
    toSound x = Sound $ (flip NodeSource) 1 $ AdditiveNode $ fmap (\(f,g)->OscillatorNode $ Oscillator Sine f g) x
    sounds = fmap toSound [ex0,ex1,ex2,ex3,ex4,ex5]
