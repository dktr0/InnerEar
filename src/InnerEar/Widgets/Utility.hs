module InnerEar.Widgets.Utility where


import qualified Data.Map as M
import Data.Maybe
import Data.Bool (bool)
import Reflex
import Reflex.Dom

-- | dynE is like dyn from Reflex, specialized for widgets that return
-- events. A dynamic argument updates the widget, and the return value is
-- already flattened to just being the events returned by the child widget.
dynE :: MonadWidget t m => Dynamic t (m (Event t a)) -> m (Event t a)
dynE x = dyn x >>= switchPromptly never

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

