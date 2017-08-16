module InnerEar.Widgets.Test where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg
import Data.Map (Map)
import qualified Data.Map as M
import Data.FileEmbed
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)
import Control.Monad
import Control.Monad.IO.Class


import InnerEar.Widgets.Utility
import InnerEar.Types.Request
import InnerEar.Types.Response
import Reflex.Synth.Types
import Reflex.Synth.Synth
import InnerEar.Widgets.Bars

labelBarButton' :: MonadWidget t m => String ->  Dynamic t String -> Dynamic t Float -> m (Event t ())
labelBarButton' label buttonString barHeight = do
   labelsForBars label
   let barWidth = constDyn 30
   drawBarCSS barHeight barWidth
   question <- dynButton buttonString -- m (Event t ())
   return (question)


testWidget :: MonadWidget t m
  => Event t [Response] -> m (Event t Request,Event t Sound,Event t ())
testWidget responses = el "div" $ do
  source <- sourceWidget
  sound <- mapDyn ((flip FilteredSound) (Filter Lowpass 100 1 1)) source
  playButton <-  button "play sound"
  let sounds = tagDyn sound playButton
  let label = "my label"
  let buttonString = constDyn "a question"
  barHeight <- count playButton
  labelBarButton' label buttonString barHeight
  home <- button "back to splash page"
  return (never,sounds,home)



-- Do not delete!
testSoundWidget::MonadWidget t m => Event t [Response] -> m (Event t Request, Event t Sound, Event t ())
testSoundWidget _ = el "div" $ do
  let attrs = constDyn $ M.fromList $ zip ["cols"] ["80"]
  x <- textArea $ def & textAreaConfig_attributes .~ attrs
  eval <- button "eval"
  let text = _textArea_value x
  maybeSound <- mapDyn (\y->maybe NoSound id (readMaybe y::Maybe Sound)) text --dyn Maybe Sound
  mapDyn show maybeSound >>= dynText
  let sounds = tagDyn maybeSound eval
  home <- button "back to splash page"
  return (never,sounds,home)
