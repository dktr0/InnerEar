module InnerEar.Widgets.Test where

import Reflex
import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as M
import Data.FileEmbed
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)
import Control.Monad



import InnerEar.Types.Request
import InnerEar.Types.Response
import Reflex.Synth.Types
import Reflex.Synth.Synth
import InnerEar.Widgets.Bars

testWidget :: MonadWidget t m
  => Event t [Response] -> m (Event t Request,Event t ())
testWidget responses = el "div" $ do
--  text "testpage placeholder"
  makeASound <- liftM ((FilteredSound (BufferSource (File "pinknoise.wav") 2.0) (Filter Peaking 400 1 1)) <$) $ button "Pinknoise Peak 400 1 1"
--  makeASound'' <- liftM ((FilteredSound (BufferSource (File "pinknoise.wav") 2.0) (Filter Peaking 900 1 1)) <$) $ button "Pinknoise Peak 400 1 1"
--  performSound $ leftmost [makeASound,makeASound', makeASound'']
  score <- count makeASound
--  score' <- count makeASound'
--  score'' <- count makeASound''
  drawBar score
--  drawBar' score'
--  drawBar'' score''
  questionLabel <- mapDyn show score
  dynButton questionLabel
  -- labelBarButton "myLabel" 100.0 questionLabel score

  home <- button "back to splash page"
  return (never,home)


testSoundWidget::MonadWidget t m => Event t [Response] -> m (Event t Request, Event t ())
testSoundWidget _ = el "div" $ do
  let attrs = constDyn $ M.fromList $ zip ["cols"] ["80"]
  x <- textArea $ def & textAreaConfig_attributes .~ attrs
  eval <- button "eval"
  let text = _textArea_value x
  maybeSound <- mapDyn (\y->maybe NoSound id (readMaybe y::Maybe Sound)) text --dyn Maybe Sound
  --holdDyn "noChange" (fmap show (updated maybeSound)) >>= dynText
  --let ev = fmap fromJust $ ffilter isJust $ updated maybeSound
  -- <- mapDyn (\x-> if isJust x then fromJust x else NoSound maybeSound) maybeSound
  mapDyn show maybeSound >>= dynText
  performSound $ tagDyn maybeSound eval
  home <- button "back to splash page"
  return (never,home)
