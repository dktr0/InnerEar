module InnerEar.Widgets.AnswerButton where

import Reflex
import Reflex.Dom
import Data.Map
import Control.Monad

import InnerEar.Widgets.Utility
import InnerEar.Widgets.Bars


data AnswerButtonMode =
  NotPossible |
  Possible |
  IncorrectDisactivated | -- should become IncorrectChosen (i.e. can't press anymore)
  IncorrectActivated | -- should become IncorrectReactivated (i.e. can press to make sound, still marked as wrong)
  Correct deriving (Eq,Show) -- should become CorrectChosen
  -- should add: CorrectMissed (i.e. what correct answer becomes when tries run out)

buttonDynCss :: MonadWidget t m => String -> Dynamic t String -> m (Event t ())
buttonDynCss label cssClass =  elClass "div" "answerButtonWrapper" $ do
  cssClass' <- mapDyn (singleton "class") cssClass
  (element, _) <- elDynAttr' "button" cssClass' $ text label -- m
  return $ domEvent Click element  -- domEvent :: EventName en -> a -> Event t (EventResultType en)

answerButton:: MonadWidget t m => String -> Dynamic t AnswerButtonMode -> a -> m (Event t a)
answerButton buttonString buttonMode x = elClass "div" "answerButtonWrapper" $ do
  c <- mapDyn modeToClass buttonMode
  ev <-  liftM (x <$) $ buttonDynCss buttonString c
  return $ attachWithMaybe f (current buttonMode) ev
  where
    f (NotPossible) _ = Nothing
    f a b = Just b

--answerButton' :: MonadWidget t m => String -> Dynamic t AnswerButtonMode -> m (Event t ())
--answerButton' buttonString buttonMode = do
--  curClass <- mapDyn modeToClass buttonMode
--  buttonDynCss buttonString curClass

modeToClass :: AnswerButtonMode -> String
modeToClass NotPossible = "notPossibleButton"
modeToClass Possible = "possibleButton"
modeToClass IncorrectDisactivated = "incorrectDisactivatedButton"
modeToClass Correct = "correctButton"
modeToClass IncorrectActivated = "incorrectActivatedButton"
