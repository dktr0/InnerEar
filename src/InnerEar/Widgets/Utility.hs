{-# LANGUAGE RecursiveDo #-}

module InnerEar.Widgets.Utility where


import qualified Data.Map as M
import Data.Maybe
import Data.Bool (bool)
import Data.List (elemIndex)
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

elDynClass::MonadWidget t m => String -> Dynamic t String -> m a -> m a
elDynClass s c b = mapDyn (singleton "class") c >>= (flip $ elDynAttr s) b

flippableDyn :: MonadWidget t m => m () -> m () -> Dynamic t Bool -> m ()
flippableDyn b1 b2 x = mapDyn (bool b1 b2) x >>= dyn >> return ()

flippableDynE :: MonadWidget t m => m (Event t a) -> m (Event t a) -> Dynamic t Bool -> m (Event t a)
flippableDynE b1 b2 x = mapDyn (bool b1 b2) x >>= dynE

visibleWhen :: MonadWidget t m => Dynamic t Bool -> m a -> m a
visibleWhen visible builder = do
  attrs <- mapDyn (bool (M.singleton "style" "display: none;") (M.singleton "style" "display: inline;")) visible
  elDynAttr "div" attrs builder

flippableWidget :: MonadWidget t m => m a -> m a -> Bool -> Event t Bool -> m (Dynamic t a)
flippableWidget b1 b2 i e = widgetHold (bool b1 b2 i) $ fmap (bool b1 b2) e


-- Button With Dynamic attributes
buttonDynAttrs :: MonadWidget t m => String -> a -> Dynamic t (M.Map String String)-> m (Event t a)
buttonDynAttrs s val attrs = do
  (e, _) <- elDynAttr' "button" attrs $ text s
  let event = domEvent Click e
  return $ fmap (const val) event

buttonClass::MonadWidget t m => String -> String -> m( Event t () )
buttonClass s c = do
  (e, _) <- elAttr' "button" (singleton "class" c) $ text s
  return $ domEvent Click e

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


hideableWidget::MonadWidget t m => Dynamic t Bool -> String -> m a -> m a
hideableWidget b c m = do
  attrs <- mapDyn (bool (fromList [("hidden","true"),("class",c)]) (singleton "class" c)) b
  elDynAttr "div" attrs  m

radioWidget::( MonadWidget t m, Ord v, Eq v)=> M.Map String v -> Maybe v -> m (Dynamic t (Maybe v), Event t (Maybe v))
radioWidget radioMap iVal = el "table" $ do
  let checked = M.fromList $ zip ["checked","type"] ["checked","radio"]
  let invertedRadioMap = M.fromList $ zip (M.elems radioMap) (M.keys radioMap) -- Inverted so things are displayed in their natural ordering rather than string ordering. so -20db comes before -100db for instance
  evs <- sequence $ M.mapWithKey (\val str-> el "tr" $ do
    ev <-el "td" $ mdo
      attrs <- holdDyn (bool (M.singleton "type" "radio") checked $ (maybe False (==val) iVal)) (checked <$ checkEv)
      (e,_) <-elDynAttr' "input" attrs (return ())
      let checkEv = domEvent Click e
      return (Just val <$ checkEv)
    el "td" $ text str
    return ev)  invertedRadioMap   -- Sorry.....
  dynVal <- holdDyn iVal $ leftmost $ M.elems evs  --
  return (dynVal, updated dynVal)


-- Alternative to just using dropdown that circumvents a glitch where choosing new dropdown items just resorts the default
safeDropdown::(MonadWidget t m, Ord k)=> k -> Map k String -> Dynamic t (Map String String) -> Event t k -> m (Dynamic t k, Event t k)
safeDropdown iVal ddMap attrs setEv = do
  let ddMapInt = fromList $ zip [0::Int,1..] (elems ddMap)
  let valuesMap = fromList $ zip [0::Int,1..] (keys ddMap)
  let initialIndex = maybe 0 id $ elemIndex iVal (keys ddMap)
  let ddConfig = DropdownConfig (fmap (maybe 0 id . (flip Data.Map.lookupIndex) ddMap) setEv) attrs
  dd <- dropdown (maybe 0 id $ Data.Map.lookupIndex iVal ddMap) (constDyn ddMapInt) ddConfig
  -- dd <- dropdown initialIndex (constDyn $ fromList $ zip [0,1,2] ["a","b","c"]) ddConfig
  val <- mapDyn (maybe iVal id . (flip Data.Map.lookup) valuesMap) (_dropdown_value dd)
  let updateEv = fmap (maybe iVal id . (flip Data.Map.lookup) valuesMap) (_dropdown_change dd)
  return (val, updateEv)

-- [Not tested]
listOfDynToDynList :: MonadWidget t m => [Dynamic t a] -> m (Dynamic t [a])
listOfDynToDynList xs = do
  let m = constDyn $ fromList $ zip [1::Int,2..] xs -- Dynamic t (Map a (Dynamic t b))
  let m' = joinDynThroughMap m
  mapDyn elems m'
