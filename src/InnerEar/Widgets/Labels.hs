{-# LANGUAGE ScopedTypeVariables #-}

module InnerEar.Widgets.Labels where

import Reflex
import Reflex.Dom
import Data.Map as M
import Control.Monad
import Prelude as P
import InnerEar.Widgets.Utility
import InnerEar.Types.Score
import InnerEar.Types.GScore

--Labels with CSS style to be used above bars
labelsForBars :: MonadWidget t m => String -> m ()
labelsForBars s = do
   elClass "div" "text" $ text s
   return ()

-- A Dynamic label for percentage
dynPercentage :: MonadWidget t m => Dynamic t Int -> m ()
dynPercentage p = do
   p' <- mapDyn show p
   el "div" $ do
     dynText p'
     return ()

--A dynamic label for Float percentage
dynPercentageFloat :: MonadWidget t m => Dynamic t String -> Dynamic t Float -> m ()
dynPercentageFloat c p = do
   c' <- mapDyn (singleton "class") c
   p' <- mapDyn show p
   elDynAttr "div" c' $ do
     dynText p'
     return ()

-- A dynamic label
dynLabelForBar :: MonadWidget t m => Dynamic t String -> Dynamic t String -> m ()
dynLabelForBar c label = do
   c' <- mapDyn (singleton "class") c
   elDynAttr "div" c' $ do
     dynText label
     return ()

--A dynamic label for count
dynCount :: MonadWidget t m => Dynamic t String -> Dynamic t Int -> m ()
dynCount c count = do
   c' <- mapDyn (singleton "class") c
   count' <- mapDyn show count
   elDynAttr "div" c' $ do
     dynText count'
     return ()

-- A dynamic label for Hz with CSS style
hzLabel :: MonadWidget t m => Dynamic t String -> String ->  m ()
hzLabel c s = do
   c' <- mapDyn (singleton "class") c -- m (Dynamic t String)
   elDynAttr "div" c' $ text s -- m ()
   return ()

-- A dynamic label for x value with css style
xLabel :: MonadWidget t m => String -> String ->  m ()
xLabel c s = elClass "div" c  $ text s



-- A dynamic label for Score with CSS style
dynScoreLabel :: forall t m. MonadWidget t m => Dynamic t String -> Dynamic t Double -> m ()
dynScoreLabel cssClass percent = do
  score' <- mapDyn (* 100) percent
  roundScore <- ((mapDyn round score') :: m (Dynamic t Int))
  score''  <- mapDyn show roundScore -- m (Dynamic t String)
  cssClass' <- mapDyn (singleton "class") cssClass  -- m (Dynamic t String)
  elDynAttr "div" cssClass' $ do
    dynText  score'' -- m ()
    return ()

    -- A dynamic label for Score with CSS style
emptyScoreLabel :: forall t m. MonadWidget t m =>  m ()
emptyScoreLabel = do
      elClass "div" "emptyScoreLabel" $ do
        text  "_" -- m ()
        return ()



-- A dynamic label for Count with CSS style
dynCountLabel :: MonadWidget t m => Dynamic t String -> Dynamic t Int -> m ()
dynCountLabel cssClass count = do
  count'  <- mapDyn show count -- m (Dynamic t String)
  cssClass' <- mapDyn (singleton "class") cssClass -- m (Dynamic t String)
  elDynAttr "div" cssClass' $ do
    dynText count' -- m ()
    return ()



--A label for "Hz"
hzMainLabel :: MonadWidget t m => String -> String -> m ()
hzMainLabel c s = elClass "div" c $ text s


-- A label for "#"
countMainLabel :: MonadWidget t m => String -> String -> m ()
countMainLabel c s = elClass "div" c $ text s

--A label for "%"

percentageMainLabel :: MonadWidget t m => String -> String -> m ()
percentageMainLabel c s = elClass "div" c $ text s

-- A dynamic label with CSS style
dynGraphLabel :: MonadWidget t m => Dynamic t String -> Dynamic t String -> m ()
dynGraphLabel c label = do
    c' <- mapDyn (singleton "class") c -- m (Dynamic t String)
    elDynAttr "div" c' $ do
      dynText label -- m ()
      return ()

gamifiedGraphLabel :: MonadWidget t m => Dynamic t String -> Dynamic t (Maybe GScore) -> m ()
gamifiedGraphLabel c score = do
  score' <- mapDyn (maybe (GScore 0.0 50.0) id) score
  score''  <- mapDyn show score' --Dynamic t Double
  class' <- mapDyn (singleton "class") c
  elDynAttr "div" class' $ do
    dynText score''
    return ()
