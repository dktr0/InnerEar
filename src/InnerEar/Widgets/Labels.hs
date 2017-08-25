{-# LANGUAGE ScopedTypeVariables #-}

module InnerEar.Widgets.Labels where

import Reflex
import Reflex.Dom
import Data.Map as M
import Control.Monad
import Prelude as P
import InnerEar.Widgets.Utility
import InnerEar.Types.Score

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

-- A dynamic label for Score with CSS style
dynLabel :: MonadWidget t m => Dynamic t String -> Dynamic t String -> m ()
dynLabel cssClass s = do
  cssClass' <- mapDyn (singleton "class") cssClass   -- m (Dynamic t String)
  elDynAttr "div" cssClass' $ do
    dynText s   -- m ()
    return ()

-- A dynamic label for Hz with CSS style
hzLabel :: MonadWidget t m => Dynamic t String -> String ->  m ()
hzLabel c s = do
   c' <- mapDyn (singleton "class") c -- m (Dynamic t String)
   elDynAttr "div" c' $ text s -- m ()
   return ()

-- A dynamic label for Score with CSS style
dynScoreLabel :: forall t m. MonadWidget t m => Dynamic t String -> Dynamic t (Score) -> m ()
dynScoreLabel cssClass score = do
  score' <- ((mapDyn ((* 100) . (\x ->  ((fromIntegral (questionsAsked x)) - (fromIntegral (falseNegatives x))) / (fromIntegral (questionsAsked x)))) score) ::  m (Dynamic t Float))   --m (Dynamic t Float)
  roundScore <- ((mapDyn round score') :: m (Dynamic t Int))
  score''  <- mapDyn ((++ "%"). show) roundScore -- m (Dynamic t String)
  cssClass' <- mapDyn (singleton "class") cssClass  -- m (Dynamic t String)
  elDynAttr "div" cssClass' $ do
    dynText  score'' -- m ()
    return ()

-- A dynamic label for Count with CSS style
dynCountLabel :: MonadWidget t m => Dynamic t String -> Dynamic t (Score) -> m ()
dynCountLabel cssClass count = do
  count' <- mapDyn questionsAsked count  -- m (Dynamic t Int)
  count''  <- mapDyn show count' -- m (Dynamic t String)
  cssClass' <- mapDyn (singleton "class") cssClass -- m (Dynamic t String)
  elDynAttr "div" cssClass' $ do
    dynText count'' -- m ()
    return ()

-- A dynamic label with CSS style
dynGraphLabel :: MonadWidget t m => Dynamic t String -> Dynamic t String -> m ()
dynGraphLabel c label = do
    c' <- mapDyn (singleton "class") c -- m (Dynamic t String)
    elDynAttr "div" c' $ do
      dynText label -- m ()
      return ()
