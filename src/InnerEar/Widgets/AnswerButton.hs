module InnerEar.Widgets.AnswerButton where

import Reflex
import Reflex.Dom
import Data.Map
import Reflex.Dom.Contrib.Widgets.Svg
import Control.Monad

import InnerEar.Widgets.Utility
import InnerEar.Widgets.Bars

data AnswerButtonMode = NotPossible | Possible | IncorrectDisactivated | IncorrectActivated  | Correct deriving (Eq)

dynButtonClass :: MonadWidget t m => Dynamic t String -> Dynamic t String -> m (Event t ())
dynButtonClass class label = do
  class' <- mapDyn Just class
  elClass "div" class' $
  dynButton

answerButton :: MonadWidget t m => Dynamic t String -> Dynamic t AnswerButtonMode -> m (Event t ())
answerButton buttonString buttonMode = do
  curClass <- mapDyn modeToClass buttonMode
  divAttrs <- mapDyn (singleton "class") curClass
  elDynAttr "div" divAttrs $ do
    dynButtonClass curClass buttonString

modeToClass :: AnswerButtonMode -> String
modeToClass NotPossible = "disabledButton"
modeToClass Possible = "enabledButton"
modeToClass IncorrectDisactivated = "incorrectDisactivatedButton"
modeToClass Correct = "correctButton"
modeToClass IncorrectActivated = "incorrectActivatedButton"
dynButtonClass :: MonadWidget t m => Dynamic t String -> Dynamic t String -> m (Event t ())
dynButtonClass = (mapDyn button) >=> dynE

dynButton :: MonadWidget t m => Dynamic t String -> m (Event t ())
dynButton = (mapDyn button) >=> dynE
