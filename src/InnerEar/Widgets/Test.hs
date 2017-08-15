module InnerEar.Widgets.Test where

import Reflex
import Reflex.Dom
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

testWidget :: MonadWidget t m
  => Event t [Response] -> m (Event t Request,Event t ())
testWidget responses = el "div" $ do

  sound <- filteredSoundWidget (constDyn $ Filter Lowpass 100 1 1)
  
  --mapDyn connectGraphToDest sound
  --soundEv <- liftM (sound <$) $ button "play sound"
  playButton <-  button "play sound"
  performSound $ tagDyn sound playButton

  soundEv <- button "soundEv button"
  --performSound $ leftmost [soundEv, fmap (const sound) fileChangeEv]
  score <- count soundEv
--  score' <- count makeASound'
--  score'' <- count makeASound''
--  drawBar score
--  drawBar' score'
--  drawBar'' score''

------first button
  questionLabel <- mapDyn show score
--  dynButton questionLabel
  labelBarButton "myLabel" questionLabel score

---second button
  pressed <- dynButton questionLabel
  someText <- holdDyn "not pressed yet" $ "pressed" <$ pressed
  dynText someText

  test

  home <- button "back to splash page"
  return (never,home)



-- Do not delete!
testSoundWidget::MonadWidget t m => Event t [Response] -> m (Event t Request, Event t ())
testSoundWidget _ = el "div" $ do
  let attrs = constDyn $ M.fromList $ zip ["cols"] ["80"]
  x <- textArea $ def & textAreaConfig_attributes .~ attrs
  eval <- button "eval"
  let text = _textArea_value x
  maybeSound <- mapDyn (\y->maybe NoSound id (readMaybe y::Maybe Sound)) text --dyn Maybe Sound
  mapDyn show maybeSound >>= dynText
  performSound $ tagDyn maybeSound eval
  home <- button "back to splash page"
  return (never,home)


--drawBar ::  MonadWidget t m =>  Dynamic t Int -> m ()
--drawBar x =  do
-- let svg = Just "http://www.w3.org/2000/svg"
-- let svgAttrs = fromList [("width", "100px")
--                , ("height", "200px")
--                , ("viewBox", "0 0 300 200") ]
-- --elDynAttr "200px" svgAttrs $ do el "height" $ x
-- elWith "svg" (ElConfig svg svgAttrs) $ do
--   elWith "rect" (ElConfig svg (M.fromList [("width", "100%"), ("height", "100%"), ("fill", "red")])) (return ())
