module InnerEar.Widgets.UserMedia where

import Reflex
import Reflex.Dom
import Reflex.Synth.Types
import Reflex.Synth.Synth
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common
import Control.Monad
import Control.Monad.IO.Class(liftIO)
import qualified Data.Map as M

import InnerEar.Types.Score


userMediaWidget'::MonadWidget t m => String -> Dynamic t Filter -> m ()
userMediaWidget' s filt = do
  bufferLoadEv <- bufferInput s
  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ [(1::Int,"Natural"),(2,"With filter")])
           (WidgetConfig {_widgetConfig_initialValue= Just 1
                         ,_widgetConfig_setValue = never
                         ,_widgetConfig_attributes = constDyn M.empty})
  radioSelection <- mapDyn (maybe 2 id) (_hwidget_value radioWidget)
  let source = NodeSource (BufferNode $ LoadedFile s) 2
  dynSound <- combineDyn (\x f-> if x ==1 then Sound source else FilteredSound source f) radioSelection filt
  soundEv <- button "play"
  performSound $ tagDyn dynSound soundEv

userMediaWidget::MonadWidget t m => String -> Dynamic t Filter -> m ()
userMediaWidget s filt = do
  elClass "audio" s (return ())
  bufferLoadEv <- bufferInput' s
  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ [(1::Int,"Natural"),(2,"With filter")])
  			(WidgetConfig {_widgetConfig_initialValue= Just 1
  						 ,_widgetConfig_setValue = never
  						 ,_widgetConfig_attributes = constDyn M.empty})
  radioSelection <- mapDyn (maybe 2 id) (_hwidget_value radioWidget)
  dynNode <- combineDyn (\x f-> if x==1 then GainNode 1 else FilterNode f) radioSelection filt
  let source = NodeSource (MediaNode s) 2
  loadedSound <- toggle False $ (True <$) bufferLoadEv
  --gate:: Behavior Bool -> Event a -> Event a
  let updateNodeEv = gate (current loadedSound) $ updated dynNode
  holdAndConnectSound source updateNodeEv


