module InnerEar.Widgets.Config where

import Data.Map
import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common

import Reflex.Synth.Types
import InnerEar.Widgets.UserMedia



dynRadioConfigWidget::(MonadWidget t m, Eq c, Show c) => String -> Map String c -> c -> m (Dynamic t c, Dynamic t Source, Event t (Maybe a))
dynRadioConfigWidget inputID configMap iConfig = elClass "div" "configWidget" $ do
  config <- elClass "div" "radioConfigWidget" $ do
    text "Exercise Configuration:  "

    let radioConfig = WidgetConfig never (Just iConfig)  (constDyn empty)
    radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ fmap  (\(k,v)->(v,k)) $ toList configMap)  radioConfig
    mapDyn (maybe iConfig id) (_hwidget_value radioWidget)
  (source,playReference) <- sourceWidget inputID
  return (config, source, Nothing <$ playReference)
