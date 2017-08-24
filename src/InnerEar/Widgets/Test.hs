module InnerEar.Widgets.Test where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg
import Data.Map (Map)
import qualified Data.Map as M
import Data.FileEmbed
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)
import Control.Monad
import Control.Monad.IO.Class
import Data.Map

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


displaySpectrumEvaluation'' :: MonadWidget t m => Dynamic t String -> Dynamic t (Map Int Score) -> m ()
displaySpectrumEvaluation'' graphLabel score= elClass "div" "specEvalWrapper" $ do
  dynGraphLabel (constDyn "graphLabel") graphLabel
  --elClass "div" "hzTitleLabel" $ text "Hz"
  elClass "div" "countTitleLabel" $ text "#"

  let labels = Prelude.map (++ " Hz") (Prelude.map show [31 :: Int, 63 :: Int, 125 :: Int, 250 :: Int, 500 :: Int, 1 :: Int, 2 :: Int, 4 :: Int, 8 :: Int, 16 :: Int])
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
  band2ScoreBar <- scoreBar band2Score band2Hz
  band3ScoreBar <- scoreBar band3Score band3Hz
  band4ScoreBar <- scoreBar band4Score band4Hz
  band5ScoreBar <- scoreBar band5Score band5Hz
  band6ScoreBar <- scoreBar band6Score band6Hz
  band7ScoreBar <- scoreBar band7Score band7Hz
  band8ScoreBar <- scoreBar band8Score band8Hz
  band9ScoreBar <- scoreBar band9Score band9Hz
  return ()

testWidget :: MonadWidget t m
  => Event t [Response] -> m (Event t Request,Event t Sound,Event t ())
testWidget responses = el "div" $ do
  let m = constDyn (M.fromList [(0, (Score 1 3 9)), (1, (Score 2 3 8)), (2, (Score 3 3 7)), (3, (Score 4 3 6)), (4, (Score 5 3 5)), (5, (Score 6 3 4)), (6, (Score 7 3 3)), (7, (Score 8 3 2)), (8, (Score 9 3 1)), (9, (Score 10 3 0))])
  displaySpectrumEvaluation'' (constDyn"My graph") m
  home <- button "back to splash page"
  return (never,never,home)

jamieTestWidget::MonadWidget t m => Event t [Response] -> m (Event t Request,Event t Sound,Event t ())
jamieTestWidget _ = el "div" $ do
  let oscs = fmap (\(f,g)-> OscillatorNode $ Oscillator Sine f g) $ zip (fmap (220*) [1,2,3,4,5]) (repeat 0.5) -- [Node] (all OscillatorNode)
  let sound = FilteredSound (NodeSource (AdditiveNode oscs) 4) (Filter Lowpass 400 1 1)
  soundEv <- liftM (sound <$) $ button "play additive synth"
  performSound soundEv
  home <- button "back to splash page"
  return (never,never,home)
