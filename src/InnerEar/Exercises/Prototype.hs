{-# LANGUAGE RecursiveDo #-}

module InnerEar.Exercises.Prototype where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import System.Random
import Data.Maybe (fromJust)
import Data.Bool (bool)

import InnerEar.Widgets.Utility
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Widgets.Bars
import InnerEar.Widgets.Test
import InnerEar.Types.Exercise
import InnerEar.Types.ExerciseId
import InnerEar.Types.ExerciseNavigation

-- | We deliberately don't give an explicit type signature to prototypeExercise
-- so that this can change as the definition of createExerciseWidget changes.
-- But see the comment below about the typing of the Exercise value within this definition...

prototypeExercise = createExerciseWidget $ Exercise {
  exerciseId = PrototypeExercise,
  defaultConfig = (),
  configWidget = prototypeConfigWidget,
  generateQuestion = prototypeGenerateQuestion,
  questionWidget = prototypeQuestionWidget,
  reflectiveQuestion = Just "Please write some brief text reflecting on your experience:"
  } :: MonadWidget t m => Exercise t m () Int Int ()

-- | The Exercise value above is a complete definition of an exercise that can be
-- "translated" into functioning Reflex-Dom widgets in the context of our system.
-- We explicitly type the Exercise value in order to be clear about the signatures
-- required for the individual components of the exercise (referenced in the value
-- above and defined fully below. The t and the m are required by Reflex. The next
-- four types (after the m) represent the exercise's configuration, question, answer
-- (used both for the correct answer and the user's actual answer) and evaluation (i.e.
-- running score, etc).

prototypeConfigWidget :: MonadWidget t m => () -> m (Event t ())
prototypeConfigWidget _ = do
  text "placeholder for prototype config widget"
  button "next"

prototypeGenerateQuestion :: () -> [Datum () Int Int ()] -> IO Int
prototypeGenerateQuestion _ _ = getStdRandom ((randomR (0,9))::StdGen -> (Int,StdGen))

prototypeQuestionWidget :: MonadWidget t m => Event t (Int,Int) -> m (Event t (Datum () Int Int ()),Event t Sound,Event t ExerciseNavigation)
prototypeQuestionWidget newQuestion = mdo
  let sounds = M.fromList $ zip [0::Int,1..] $ fmap (FilteredSound (BufferSource (File "pinknoise.wav") 2.0)) filters
  let radioButtonMap = (zip [0::Int,1..] ["100 Hz","200 Hz","300 Hz","400 Hz","500 Hz","600 Hz","700 Hz","800 Hz","900 Hz","1000 Hz"])
  playButton <- button "Play Sound"
  radioWidget <- radioGroup (constDyn "test") (constDyn radioButtonMap)
         (WidgetConfig {_widgetConfig_initialValue= Nothing
                       ,_widgetConfig_setValue = never
                       ,_widgetConfig_attributes = constDyn M.empty})
  submitButton <- buttonDynAttrs "submit" () submitAttrs
  userAnswer <- holdDyn Nothing $ tagDyn (_hwidget_value radioWidget) submitButton
  nextButtonWidget <- flippableWidget  (return never) (button "next") False (leftmost [(True <$) submitButton, (False <$) nextButton])
  let nextButton = switchPromptlyDyn nextButtonWidget
  submitAttrs <- toggle True (leftmost [submitButton,nextButton]) >>= mapDyn (\x-> if x then M.empty else "disabled"=:"disabled")
  soundNum <- holdDyn 0 newQuestion
  answerIsCorrect <- combineDyn (\x y-> maybe False (x==) y) soundNum userAnswer
  correctText <- combineDyn (\x y-> if x then "Correct!" else "The correct answer was "++(fromJust $ M.lookup y $ M.fromList radioButtonMap)) answerIsCorrect soundNum
  flippableWidget (text "") (dynText correctText) False (leftmost [(True <$) submitButton, (False <$) nextButton])
  sound <- mapDyn (\x-> fromJust $ M.lookup x sounds) soundNum
  el "div" $ mapDyn (\x-> "Current sound is:  " ++show x) sound >>=dynText
  backToConfigure <- (Configure <$) <$> button "Configure"
  onToReflect <- (Reflect <$) <$> button "Reflect"
  let navEvents = leftmost [backToConfigure,onToReflect]
  return (never,tagDyn sound playButton,navEvents)

filters:: [Filter]
filters = fmap (flip ((flip (Filter Peaking)) 5) 40) [100,200,300,400,500,600,700,800,900,1000]




prototypeQuestionWidget2 :: MonadWidget t m => Event t (Int,Int) -> m (Event t (Datum () Int Int ()),Event t Sound,Event t ExerciseNavigation)
prototypeQuestionWidget2 newQuestion = do
  question <- holdDyn
