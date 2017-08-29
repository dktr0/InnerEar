module InnerEar.Widgets.Test where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg
import Data.Map as M
import Data.FileEmbed
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)
import Control.Monad
import Control.Monad.IO.Class


import InnerEar.Widgets.Utility
import InnerEar.Widgets.AnswerButton
import InnerEar.Types.Request
import InnerEar.Types.Response
import Reflex.Synth.Types
import Reflex.Synth.Synth
import InnerEar.Widgets.Bars
import InnerEar.Widgets.AnswerButton
import InnerEar.Widgets.SpecEval
import InnerEar.Widgets.Labels
import InnerEar.Types.Score
import InnerEar.Types.Frequency


testWidget :: MonadWidget t m
  => Event t [Response] -> m (Event t Request,Event t Sound,Event t ())
testWidget responses = el "div" $ do
--  scoreBar (constDyn . Just $ (Score 1 3 9)) "31 hz"
--  let m = constDyn (M.fromList [((F 250 "250 Hz"), (Score 1 2 9)), ((F 500 "500 Hz"), (Score 2 3 8)), ((F 1000 "1 KHz"), (Score 3 3 7)), ((F 2000 "2 KHz"), (Score 4 3 6))])
--  displayCurrentSpectrumEvaluation (constDyn "Current Performance") m
  displaySpectrumEvaluationGraphs
  home <- button "back to splash page"
  return (never,never,home)
