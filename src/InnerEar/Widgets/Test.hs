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
import InnerEar.Widgets.RangePicker

testWidget :: MonadWidget t m
  => Event t [Response] -> m (Event t Request,Event t Sound,Event t ())
testWidget responses = elClass "div" "excerciseWrapper" $ do
  rangePicker
  let possibilities = ["Q1", "Q2", "Q3", "Q4","Q5"]
  let scoreMap =  constDyn (M.fromList [("Q1", (Score 1 2 9)), ("Q2", (Score 2 2 8)), ("Q3", (Score 3 2 7)), ("Q4", (Score 4 2 6)), ("Q5", (Score 10 2 0))])
  displayMultipleChoiceEvaluationGraph' "Session performance" "" possibilities scoreMap
  home <- button "back to splash page"
  return (never,never,home)
