module InnerEar.Widgets.AnswerButton where

import Reflex
import Reflex.Dom
import Data.Map
import Reflex.Dom.Contrib.Widgets.Svg
import Control.Monad

import InnerEar.Widgets.Utility
import InnerEar.Widgets.Bars

data AnswerButtonMode = NotPossible | Possible | IncorrectDisactivated | IncorrectActivated  | Correct deriving (Eq,Show)

dynButtonClass :: MonadWidget t m => Dynamic t String -> Dynamic t String -> m (Event t ())
dynButtonClass c label = do
  c' <- mapDyn (singleton "class") c
  elDynAttr "div" c' $ dynButton label

answerButton:: MonadWidget t m => Dynamic t String -> Dynamic t AnswerButtonMode -> a -> m (Event t a)
answerButton buttonString buttonMode x = do
  curClass <- mapDyn modeToClass buttonMode
  ev <- clickableDivDynClass buttonString curClass x
  return $ attachWithMaybe f (current buttonMode) ev
  where
    f (NotPossible) _ = Nothing
    f a b = Just b

answerButton' :: MonadWidget t m => Dynamic t String -> Dynamic t AnswerButtonMode -> m (Event t ())
answerButton' buttonString buttonMode = do
  curClass <- mapDyn modeToClass buttonMode
  divAttrs <- mapDyn (singleton "class") curClass
  elDynAttr "div" divAttrs $ do
    dynButtonClass curClass buttonString

modeToClass :: AnswerButtonMode -> String
modeToClass NotPossible = "notPossibleButton"
modeToClass Possible = "possibleButton"
modeToClass IncorrectDisactivated = "incorrectDisactivatedButton"
modeToClass Correct = "correctButton"
modeToClass IncorrectActivated = "incorrectActivatedButton"
