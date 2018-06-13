module InnerEar.Widgets.AnswerButton where

import Reflex
import Reflex.Dom
import Data.Map
import Control.Monad
import Data.Bool

import InnerEar.Widgets.Utility
import InnerEar.Widgets.Bars

data AnswerButtonMode =
  NotPossible |
  Possible |
  IncorrectDisactivated | -- should become IncorrectChosen (i.e. can't press anymore)
  IncorrectActivated | -- should become IncorrectReactivated (i.e. can press to make sound, still marked as wrong)
  Correct | -- should become CorrectChosen
  CorrectMissed -- (i.e. what correct answer becomes when tries run out)
  deriving (Eq,Show)

class Buttonable a where
  makeButton :: MonadWidget t m => a -> Dynamic t AnswerButtonMode -> m (Event t a)

showAnswerButton :: (MonadWidget t m, Show a) => a -> Dynamic t AnswerButtonMode -> m (Event t a)
showAnswerButton a m = answerButton (show a) m a

revealableButton :: MonadWidget t m => String -> String -> Dynamic t Bool -> m (Event t ())
revealableButton label classWhenVisible isVisible = do
  cssClass <- mapDyn (\x -> bool (classWhenVisible ++ " makeButtonInvisible") classWhenVisible x) isVisible
  buttonDynCss label cssClass

buttonDynCss :: MonadWidget t m => String -> Dynamic t String -> m (Event t ())
buttonDynCss label cssClass =  do
  cssClass' <- mapDyn (singleton "class") cssClass
  (element, _) <- elDynAttr' "button" cssClass' $ text label -- m
  return $ domEvent Click element  -- domEvent :: EventName en -> a -> Event t (EventResultType en)

answerButton :: MonadWidget t m => String -> Dynamic t AnswerButtonMode -> a -> m (Event t a)
answerButton buttonString buttonMode x = do
  c <- mapDyn modeToClass buttonMode
  ev <-  liftM (x <$) $ buttonDynCss buttonString c
  return $ attachWithMaybe f (current buttonMode) ev
  where
    f (NotPossible) _ = Nothing
    f (IncorrectDisactivated) _ = Nothing
    f a b = Just b

-- looks and acts like our answerButton but contains arbitrary children...
-- answerButton' :: MonadWidget t m => Dynamic t AnswerButtonMode -> a -> m b -> m (Event t a)

modeToClass :: AnswerButtonMode -> String
modeToClass NotPossible = "notPossibleButton"
modeToClass Possible = "possibleButton"
modeToClass IncorrectDisactivated = "incorrectDisactivatedButton"
modeToClass Correct = "correctButton"
modeToClass CorrectMissed = "correctButton" -- placeholder: this needs to change to a unique style
modeToClass IncorrectActivated = "incorrectActivatedButton"
