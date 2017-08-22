module InnerEar.Widgets.SpecEval where

import Reflex
import Reflex.Dom
import Data.Map as M
import Reflex.Dom.Contrib.Widgets.Svg
import Control.Monad
import Data.Maybe (isJust)

import InnerEar.Widgets.Utility
import InnerEar.Widgets.Bars
import InnerEar.Types.Score

dynLabel :: MonadWidget t m => Dynamic t String -> Dynamic t String -> m ()
dynLabel cssClass s = do
  cssClass' <- mapDyn (singleton "class") cssClass   -- m (Dynamic t String)
  elDynAttr "div" cssClass' $ do
    dynText s   -- m ()
    return ()

--Labels with CSS style to be used above bars
hzLabel :: MonadWidget t m => Dynamic t String -> String ->  m ()
hzLabel c s = do
   c' <- mapDyn (singleton "class") c -- m (Dynamic t String)
   elDynAttr "div" c' $ text (show s) -- m ()
   return ()

dynScoreLabel :: MonadWidget t m => Dynamic t String -> Dynamic t (Score) -> m ()
dynScoreLabel cssClass score = do
  score' <- mapDyn  (\x ->  ((fromIntegral (questionsAsked x) :: Float) - (fromIntegral (falseNegatives x) :: Float)) / (fromIntegral (questionsAsked x) :: Float)) score   --m (Dynamic t Double)
  score''  <- mapDyn show score' -- m (Dynamic t String)
  cssClass' <- mapDyn (singleton "class") cssClass  -- m (Dynamic t String)
  elDynAttr "div" cssClass' $ do
    dynText score'' -- m ()
    return ()

dynCountLabel :: MonadWidget t m => Dynamic t String -> Dynamic t (Score) -> m ()
dynCountLabel cssClass count = do
  count' <- mapDyn questionsAsked count  -- m (Dynamic t Int)
  count''  <- mapDyn show count' -- m (Dynamic t String)
  cssClass' <- mapDyn (singleton "class") cssClass -- m (Dynamic t String)
  elDynAttr "div" cssClass' $ do
    dynText count'' -- m ()
    return ()

--A dynamic bar with css style and in-line attributes
dynBarCSS :: MonadWidget t m =>  Dynamic t (Score) -> Dynamic t Float -> m ()
dynBarCSS score barWidth = do
  elClass "div" "flex-container" $ do
    svgClass "svg" "svgS" $ do
      let posX = constDyn $ negate 30 -- Dynamic t Int
      let posY = constDyn $ negate 200  --Dynamic t Int
      barHeight <- mapDyn  (\x ->  ((fromIntegral (questionsAsked x) :: Float) - (fromIntegral (falseNegatives x) :: Float)) / (fromIntegral (questionsAsked x) :: Float)) score   --m (Dynamic t Int)
      barHeight' <- mapDyn (*200) barHeight -- m (Dynamic t Float)
      barWidth' <- mapDyn (*1) barWidth -- m (Dynamic t Float)
      let c = constDyn "test" --Dynamic t String
      let t = constDyn "rotate (180)" --Dynamic t String
      rectDynCSS posX posY barWidth' barHeight' t c  -- m ()

scoreBar :: MonadWidget t m => Dynamic t (Maybe Score) -> String ->  m ()
scoreBar score hz = do
    bool <- mapDyn isJust score
    flippableDyn (return ()) (do
      barHeight <- mapDyn (maybe (Score 0 0 0) id) score -- Dynamic t Int
      dynBarCSS barHeight (constDyn 30) -- m ()
      scoreLabel <- mapDyn (maybe (Score 0 0 0) id) score
      dynScoreLabel (constDyn "scoreLabel") scoreLabel -- m()
      hzLabel (constDyn "dynLabel") hz
      countLabel <- mapDyn (maybe (Score 0 0 0) id) score
      dynCountLabel (constDyn "countLabel") countLabel) bool --m ()
    return ()

-- A dynamic label with CSS style
dynGraphLabel :: MonadWidget t m => Dynamic t String -> Dynamic t String -> m ()
dynGraphLabel c label = do
  c' <- mapDyn (singleton "class") c -- m (Dynamic t String)
  elDynAttr "div" c' $ do
    dynText label -- m ()
    return ()

displaySpectrumEvaluation :: MonadWidget t m => Dynamic t String -> Dynamic t (Map Int Score) -> m ()
displaySpectrumEvaluation graphLabel score= do
  dynGraphLabel (constDyn "graphLabel") graphLabel
  let labels = ["31 Hz","63 Hz","125 Hz","250 Hz","500 Hz","1 kHz","2 kHz","4 kHz","8 kHz","16 kHz"]

  let band0Hz = (!!0) labels -- String
  let band1Hz = (!!1) labels
  let band2Hz = (!!2) labels
  let band3Hz = (!!3) labels
  let band4Hz = (!!4) labels
  let band5Hz = (!!5) labels
  let band6Hz = (!!6) labels
  let band7Hz = (!!7) labels
  let band8Hz = (!!8) labels
  let band9Hz = (!!9) labels

  band0Score <- mapDyn (M.lookup 0) score --  Dynamic t (Score) ?
  band1Score <- mapDyn (M.lookup 1) score
  band2Score <- mapDyn (M.lookup 2) score
  band3Score <- mapDyn (M.lookup 3) score
  band4Score <- mapDyn (M.lookup 4) score
  band5Score <- mapDyn (M.lookup 5) score
  band6Score <- mapDyn (M.lookup 6) score
  band7Score <- mapDyn (M.lookup 7) score
  band8Score <- mapDyn (M.lookup 8) score
  band9Score <- mapDyn (M.lookup 9) score

  band0ScoreBar <- scoreBar band0Score band0Hz -- m ()
  band1ScoreBar <- scoreBar band1Score band1Hz
  band2ScoreBar <- scoreBar band3Score band2Hz
  band3ScoreBar <- scoreBar band3Score band3Hz
  band4ScoreBar <- scoreBar band4Score band4Hz
  band5ScoreBar <- scoreBar band5Score band5Hz
  band6ScoreBar <- scoreBar band6Score band6Hz
  band7ScoreBar <- scoreBar band7Score band7Hz
  band8ScoreBar <- scoreBar band8Score band8Hz
  band9ScoreBar <- scoreBar band9Score band9Hz
  return ()
