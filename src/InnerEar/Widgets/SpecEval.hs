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
  maybeScore<- mapDyn (mapKeys (freqAsString) . fmap (\v-> case v of (Score 0 0 0)-> Nothing;otherwise->Just v)) score
  listWithKey maybeScore (flip scoreBar)
  return ()
