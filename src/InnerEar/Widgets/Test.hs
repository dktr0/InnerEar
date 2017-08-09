module InnerEar.Widgets.Test where

import Reflex
import Reflex.Dom

import InnerEar.Types.Request
import InnerEar.Types.Response
import Reflex.Synth.Types
import Reflex.Synth.Synth
import Control.Monad

import qualified Data.Map as M

testWidget :: MonadWidget t m
  => Event t Response -> m (Event t Request,Event t ())
testWidget responses = el "div" $ do
  text "testpage placeholder"
  makeASound <- liftM ((FilteredSound (Tone (Saw 440) 2.0) (PeakingFilter 100.0 1.0 1.0)) <$) $ button "Make A Sound"
  performSynth makeASound
  home <- button "back to splash page"
  return (never,home)

--drawbar ::  MonadWidget t m => Dynamic t Int -> m ()
--drawbar score = do
--  let svg = el "svg"
--  let svgAttrs :: Map String String
--  let svgAttrs = [("width", "300px")
--                 ,("height","300px")]
--      svg $ svgAttrs $ score $ do
--       el "rect" $ M.fromList [("width", "100px"), ("height", "100px"), ("fill", "blue")]
