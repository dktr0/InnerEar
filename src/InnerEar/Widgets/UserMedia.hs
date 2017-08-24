module InnerEar.Widgets.UserMedia where

import Reflex
import Reflex.Dom
import Reflex.Synth.Types
import Reflex.Synth.Synth
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common
import qualified Data.Map as M

--import InnerEar.Widgets.Utility


userMediaWidget::MonadWidget t m => Dynamic t Filter -> m ()
userMediaWidget filt = elClass "div" "userMediaWrapper" $ do
  (source,loadEv) <- mediaElement
  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ [(1::Int,"Natural"),(2,"With filter")])
           (WidgetConfig {_widgetConfig_initialValue= Just 2
                         ,_widgetConfig_setValue = never
                         ,_widgetConfig_attributes = constDyn M.empty})
  radioSelection <- mapDyn (maybe 2 id) (_hwidget_value radioWidget)
  dynSound <- combineDyn (\f i-> if i==1 then Sound source else FilteredSound source f) filt radioSelection
  connectGraphOnEv $ leftmost [updated dynSound, tagDyn dynSound loadEv]