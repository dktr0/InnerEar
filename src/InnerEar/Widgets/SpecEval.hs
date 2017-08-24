module InnerEar.Widgets.SpecEval where

import Reflex
import Reflex.Dom
import Data.Map as M
import Control.Monad

import InnerEar.Types.Frequency
import InnerEar.Widgets.Utility
import InnerEar.Widgets.Bars
import InnerEar.Types.Score
import InnerEar.Widgets.Labels


displaySpectrumEvaluation :: MonadWidget t m => Dynamic t String -> Dynamic t (Map Frequency Score) -> m ()
displaySpectrumEvaluation graphLabel score= elClass "div" "specEvalWrapper" $ do
  dynGraphLabel (constDyn "graphLabel") graphLabel
  maybeScore<- mapDyn (mapKeys (freqAsString) . fmap maybeQuestionHasBeenAsked) score
  listWithKey maybeScore (flip scoreBar)
  return ()
  where 
    maybeQuestionHasBeenAsked (Score 0 _ 0) = Nothing
    maybeQuestionHasBeenAsked a = Just a


--displaySpectrumEvaluation :: MonadWidget t m => Dynamic t String -> Dynamic t (Map Frequency Score) -> m ()
--displaySpectrumEvaluation graphLabel score= elClass "div" "specEvalWrapper" $ do
--  dynGraphLabel (constDyn "graphLabel") graphLabel
--  let labels = ["31 Hz","63 Hz","125 Hz","250 Hz","500 Hz","1 kHz","2 kHz","4 kHz","8 kHz","16 kHz"]
--  let frequencies = zipWith F [31::Double,63,125,250,500,1000,2000,4000,8000,16000] labels -- [Frequency]
--  let band0Hz = (!!0) labels -- String
--  let band1Hz = (!!1) labels
--  let band2Hz = (!!2) labels
--  let band3Hz = (!!3) labels
--  let band4Hz = (!!4) labels
--  let band5Hz = (!!5) labels
--  let band6Hz = (!!6) labels
--  let band7Hz = (!!7) labels
--  let band8Hz = (!!8) labels
--  let band9Hz = (!!9) labels

--  band0Score <- mapDyn (M.lookup (frequencies!!0)) score --  Dynamic t (Score) ?
--  band1Score <- mapDyn (M.lookup (frequencies!!1)) score
--  band2Score <- mapDyn (M.lookup (frequencies!!2)) score
--  band3Score <- mapDyn (M.lookup (frequencies!!3)) score
--  band4Score <- mapDyn (M.lookup (frequencies!!4)) score
--  band5Score <- mapDyn (M.lookup (frequencies!!5)) score
--  band6Score <- mapDyn (M.lookup (frequencies!!6)) score
--  band7Score <- mapDyn (M.lookup (frequencies!!7)) score
--  band8Score <- mapDyn (M.lookup (frequencies!!8)) score
--  band9Score <- mapDyn (M.lookup (frequencies!!9)) score

--  band0ScoreBar <- scoreBar band0Score band0Hz -- m ()
--  band1ScoreBar <- scoreBar band1Score band1Hz
--  band2ScoreBar <- scoreBar band2Score band2Hz
--  band3ScoreBar <- scoreBar band3Score band3Hz
--  band4ScoreBar <- scoreBar band4Score band4Hz
--  band5ScoreBar <- scoreBar band5Score band5Hz
--  band6ScoreBar <- scoreBar band6Score band6Hz
--  band7ScoreBar <- scoreBar band7Score band7Hz
--  band8ScoreBar <- scoreBar band8Score band8Hz
--  band9ScoreBar <- scoreBar band9Score band9Hz
--  return ()
