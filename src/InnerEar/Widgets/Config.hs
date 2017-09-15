module InnerEar.Widgets.Config where

import Data.Map
import Reflex
import Reflex.Dom
-- import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
-- import Reflex.Dom.Contrib.Widgets.Common

import InnerEar.Widgets.UserMedia
import InnerEar.Widgets.Utility(radioWidget)
import Reflex.Synth.Types



dynRadioConfigWidget::(MonadWidget t m, Eq c, Show c, Ord c) => String -> Map String c -> c -> m (Dynamic t c, Dynamic t Source, Event t (Maybe a))
dynRadioConfigWidget inputID configMap iConfig = elClass "div" "configWidget" $ do
  config <- elClass "div" "radioConfigWidget" $ do
    text "Exercise Configuration:  "
    (dynVal, _) <- radioWidget configMap (Just iConfig)
    mapDyn (maybe iConfig id) dynVal
  (source,playReference) <- sourceWidget inputID
  return (config, source, Nothing <$ playReference)
