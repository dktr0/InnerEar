module InnerEar.Exercises.Prototype where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common 

import Control.Monad

import InnerEar.Types.Request
import InnerEar.Types.Response
import Reflex.Synth.Synth
import Reflex.Synth.Types

import InnerEar.Widgets.Test
import qualified Data.Map as M
import System.Random
import Data.Maybe (fromJust)

import InnerEar.Widgets.Bars


prototypeExercise :: MonadWidget t m
  => Event t [Response] -> m (Event t Request,Event t ())
prototypeExercise responses = el "div" $ do
  text "prototype exercise placeholder"
  makeASound <- liftM ((FilteredSound (BufferSource (File "pinknoise.wav") 2.0) (Filter Peaking 100 1 1)) <$) $ button "Pinknoise Peak 100 1 1"
  makeASound' <- liftM ((FilteredSound (BufferSource (File "pinknoise.wav") 2.0) (Filter Peaking 400 1 1)) <$) $ button "Pinknoise Peak 400 1 1"
  makeASound'' <- liftM ((FilteredSound (BufferSource (File "pinknoise.wav") 2.0) (Filter Peaking 900 1 1)) <$) $ button "Pinknoise Peak 900 1 1"  
  performSound $ leftmost [makeASound,makeASound', makeASound'']
  score <- count makeASound
  drawBar score
  drawBar' score
  drawBar''
  home <- button "back to splash page"
  return (never,home)




tenBandsExercise::MonadWidget t m => Event t Response -> m (Event t Request, Event t ())
tenBandsExercise _ = el "div" $ do
  let order = (take 10 $ randomRs (0,9) (mkStdGen 6)) :: [Int]
  let sounds = M.fromList $ zip [0::Int,1..] $ fmap (FilteredSound (BufferSource (File "pinknoise.wav") 2.0)) filters
  playButton <- button "Play Sound"
  radioWidget <- radioGroup (constDyn "test") (constDyn (zip [0::Int,1..] ["100 Hz","200 Hz","300 Hz","400 Hz","500 Hz","600 Hz","700 Hz","800 Hz","900 Hz","1000 Hz"])) 
         (WidgetConfig {_widgetConfig_initialValue= Nothing
                       ,_widgetConfig_setValue = never
                       ,_widgetConfig_attributes = constDyn M.empty})
  submitButton <- button "submit"
  submitButtonCount <- count submitButton
  currentSoundNumber <- mapDyn (\x-> order!!(mod x 10)) submitButtonCount
  lastSoundNumber <- mapDyn (\x-> order!!(mod (x-1) 10)) submitButtonCount
  userAnswer <- holdDyn Nothing $ tagDyn (_hwidget_value radioWidget) submitButton 
  answerIsCorrect <- combineDyn (\x y-> maybe False (x==) y) lastSoundNumber userAnswer
  mapDyn (\x-> if x then "last answer was correct" else "last answer was incorrect") answerIsCorrect >>= dynText
  sound <- mapDyn (\x-> fromJust $ M.lookup x sounds) currentSoundNumber
  el "div" $ mapDyn (\x-> "Current sound is:  " ++show x) sound >>=dynText
  performSound $ tagDyn sound playButton
  home <- button "back to splash page"
  return (never,home)


filters:: [Filter]
filters = fmap (flip ((flip (Filter Peaking)) 5) 40) [100,200,300,400,500,600,700,800,900,1000]