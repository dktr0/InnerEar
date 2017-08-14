{-# LANGUAGE RecursiveDo #-}

module InnerEar.Exercises.Prototype where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common

import Control.Monad
import Control.Monad.IO.Class (liftIO)


import InnerEar.Widgets.Utility
import InnerEar.Types.Request
import InnerEar.Types.Response
import Reflex.Synth.Synth
import Reflex.Synth.Types

import InnerEar.Widgets.Test
import qualified Data.Map as M
import System.Random
import Data.Maybe (fromJust)
import Data.Bool (bool)

import InnerEar.Widgets.Bars


prototypeExercise :: MonadWidget t m
  => Event t [Response] -> m (Event t Request,Event t ())
prototypeExercise responses = el "div" $ do
  text "prototype exercise placeholder"
  score <- tenBandsExercise
  drawBar score
  drawBar' score
  home <- button "back to splash page"
  return (never,home)


-- returning a 'score' (count of exercises the user got correct)
tenBandsExercise::MonadWidget t m => m (Dynamic t Int)
tenBandsExercise = el "div" $ mdo
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
  soundNumEv <- performEvent $ fmap liftIO $ (getStdRandom ((randomR (0,9))::StdGen -> (Int,StdGen)) <$)  nextButton
  
  iSoundNum <- liftIO ((getStdRandom (randomR (0,9)))::IO Int)
  soundNum <- holdDyn iSoundNum soundNumEv
  
  answerIsCorrect <- combineDyn (\x y-> maybe False (x==) y) soundNum userAnswer
  correctText <- combineDyn (\x y-> if x then "Correct!" else "The correct answer was "++(fromJust $ M.lookup y $ M.fromList radioButtonMap)) answerIsCorrect soundNum
  flippableWidget (text "") (dynText correctText) False (leftmost [(True <$) submitButton, (False <$) nextButton])
  sound <- mapDyn (\x-> fromJust $ M.lookup x sounds) soundNum
  el "div" $ mapDyn (\x-> "Current sound is:  " ++show x) sound >>=dynText
  performSound $ tagDyn sound playButton
  home <- button "back to splash page"
  count $ ffilter id (tagDyn answerIsCorrect submitButton)



filters:: [Filter]
filters = fmap (flip ((flip (Filter Peaking)) 5) 40) [100,200,300,400,500,600,700,800,900,1000]
