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
dynButtonClass c label = do
  c' <- mapDyn (singleton "class") c  --m (Dynamic t String)
  elDynAttr "div" c' $
   dynButton label  -- m (Event t ())



answerButtonVal:: MonadWidget t m => Dynamic t String -> Dynamic t AnswerButtonMode -> a -> m (Event t a)
answerButtonVal buttonString buttonMode x = do
  curClass <- mapDyn modeToClass buttonMode  -- m (Dynamic t String) ?
  divAttrs <- mapDyn (singleton "class") curClass -- m (Dynamic t String)
  elDynAttr "div" divAttrs $ do
    fmap (const x) $ dynButtonClass curClass buttonString

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
