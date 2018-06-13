module InnerEar.Widgets.Sound where

import Reflex
import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as M
import Text.Read (readMaybe)

import InnerEar.Types.Request
import InnerEar.Types.Response
import Reflex.Synth.Synth

-- TODO there is no nice way to do this for the new synth.
testSoundWidget :: MonadWidget t m => Event t [Response] -> m (Event t Request, Event t Sound, Event t ())
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
