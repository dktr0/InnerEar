module InnerEar.Widgets.Utility where


import qualified Data.Map as M
import Data.Maybe
import Data.Bool (bool)
import Data.Map
import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common
import Reflex.Synth.Types
import Reflex.Synth.Synth
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import GHCJS.DOM.Types (castToHTMLCanvasElement)
import GHCJS.DOM.EventM

-- | dynE is like dyn from Reflex, specialized for widgets that return
-- events. A dynamic argument updates the widget, and the return value is
-- already flattened to just being the events returned by the child widget.
dynE :: MonadWidget t m => Dynamic t (m (Event t a)) -> m (Event t a)
dynE x = dyn x >>= switchPromptly never

elClass'::MonadWidget t m => String -> String -> m a -> m (El t, a)
elClass' e c f = elAttr' e (M.singleton "class" c) f


flippableDyn :: MonadWidget t m => m () -> m () -> Dynamic t Bool -> m ()
flippableDyn b1 b2 x = mapDyn (bool b1 b2) x >>= dyn >> return ()

flippableDynE :: MonadWidget t m => m (Event t a) -> m (Event t a) -> Dynamic t Bool -> m (Event t a)
flippableDynE b1 b2 x = mapDyn (bool b1 b2) x >>= dynE

visibleWhen :: MonadWidget t m => Dynamic t Bool -> m a -> m a
visibleWhen visible builder = do
  attrs <- mapDyn f visible
  elDynAttr "div" attrs builder
  where
    f = bool (M.singleton "style" "display: none;") M.empty

flippableWidget :: MonadWidget t m => m a -> m a -> Bool -> Event t Bool -> m (Dynamic t a)
flippableWidget b1 b2 i e = widgetHold (bool b1 b2 i) $ fmap (bool b1 b2) e

-- Button With Dynamic attributes
buttonDynAttrs :: MonadWidget t m => String -> a -> Dynamic t (M.Map String String)-> m (Event t a)
buttonDynAttrs s val attrs = do
  (e, _) <- elDynAttr' "button" attrs $ text s
  let event = domEvent Click e
  return $ fmap (const val) event

-- with displayed text that can change
clickableDivDynClass:: MonadWidget t m => Dynamic t String -> Dynamic t String -> a -> m (Event t a)
clickableDivDynClass label c val = do
  attrs <- mapDyn (singleton "class") c
  (element, _) <- elDynAttr' "div" attrs $ dynText label
  clickEv <- wrapDomEvent (_el_element element) (onEventName Click) (mouseXY)
  return $ (val <$) clickEv



buttonVal :: (MonadWidget t m) => String -> a -> m (Event t a)
buttonVal t r = do
  x <- button t
  return $ fmap (const r) x


--Button with dynamic label. A final version that uses >=> from Control.Monad to compose together two a -> m b functions
dynButton :: MonadWidget t m => Dynamic t String -> m (Event t ())
dynButton = (mapDyn button) >=> dynE


-- [Not tested]
listOfDynToDynList :: MonadWidget t m => [Dynamic t a] -> m (Dynamic t [a])
listOfDynToDynList xs = do
  let m = constDyn $ fromList $ zip [1::Int,2..] xs -- Dynamic t (Map a (Dynamic t b))
  let m' = joinDynThroughMap m
  mapDyn elems m'
