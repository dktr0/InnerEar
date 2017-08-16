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

test :: MonadWidget t m => m ()
test = do
   let m = M.fromList [("width","800px"),("height","800px"), ("viewBox", "0 0 300 200")]
   svgAttr "svg" m $ do
    -- let n = fromList [("width","50px"),("height","100px"), ("style", "fill:red")]
     svgClass "rect" "test" $ return ()

testWidget :: MonadWidget t m
  => Event t [Response] -> m (Event t Request,Event t Sound,Event t ())
testWidget = jamieTestWidget


someoneTestWidget :: MonadWidget t m
  => Event t [Response] -> m (Event t Request,Event t Sound,Event t ())
someoneTestWidget responses = el "div" $ do
  let oscs = fmap (\(f,g)-> OscillatorNode $ Oscillator Sine f g) $ zip (fmap (220*) [1,2,3,4,5]) (repeat 0.5) -- [Node] (all OscillatorNode)
  let sound = FilteredSound (NodeSource (AdditiveNode oscs) 4) (Filter Lowpass 400 1 1)
  soundEv <- liftM (sound <$) $ button "play additive synth"
  performSound soundEv
  score <- count soundEv
  questionLabel <- mapDyn show score
  labelBarButton "myLabel" questionLabel score
  test
  home <- button "back to splash page"
  return (never,never,home)

jamieTestWidget::MonadWidget t m => => Event t [Response] -> m (Event t Request,Event t Sound,Event t ())
jamieTestWidget _ = el "div" $ do
  let oscs = fmap (\(f,g)-> OscillatorNode $ Oscillator Sine f g) $ zip (fmap (220*) [1,2,3,4,5]) (repeat 0.5) -- [Node] (all OscillatorNode)
  let sound = FilteredSound (NodeSource (AdditiveNode oscs) 4) (Filter Lowpass 400 1 1)
  soundEv <- liftM (sound <$) $ button "play additive synth"
  performSound soundEv
  home <- button "back to splash page"
  return (never,never,home)
