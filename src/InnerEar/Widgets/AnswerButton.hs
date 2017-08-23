module InnerEar.Widgets.AnswerButton where

import Reflex
import Reflex.Dom
import Data.Map
import Control.Monad

import InnerEar.Widgets.Utility
import InnerEar.Widgets.Bars

data AnswerButtonMode = NotPossible | Possible | IncorrectDisactivated | IncorrectActivated  | Correct deriving (Eq)

dynButtonClass :: MonadWidget t m => Dynamic t String -> Dynamic t String -> m (Event t ())
dynButtonClass c label = do
  c' <- mapDyn (singleton "class") c
  elDynAttr "div" c' $ dynButton label

answerButtonVal:: MonadWidget t m => Dynamic t String -> Dynamic t AnswerButtonMode -> a -> m (Event t a)
answerButtonVal buttonString buttonMode x =  elClass "div" "answerButtonWrapper" $ do
  curClass <- mapDyn modeToClass buttonMode --m (Dynamic t String)
  liftM (x <$) $ dynButtonClass curClass buttonString

answerButton :: MonadWidget t m => Dynamic t String -> Dynamic t AnswerButtonMode -> m (Event t ())
answerButton buttonString buttonMode = elClass "div" "answerButtonWrapper" $ do
  curClass <- mapDyn modeToClass buttonMode --m (Dynamic t String)
  dynButtonClass curClass buttonString

modeToClass :: AnswerButtonMode -> String
modeToClass NotPossible = "disabledButton"
modeToClass Possible = "enabledButton"
modeToClass IncorrectDisactivated = "incorrectDisactivatedButton"
modeToClass Correct = "correctButton"
modeToClass IncorrectActivated = "incorrectActivatedButton"
