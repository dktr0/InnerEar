module InnerEar.Widgets.AnswerButton where

import Reflex
import Reflex.Dom
import Data.Map
import Control.Monad
import Data.Bool
import GHCJS.DOM.EventM

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
  makeButton :: (MonadWidget t m, Show a) => a -> Dynamic t AnswerButtonMode -> m (Event t a)

showAnswerButton :: (MonadWidget t m, Show a) => a -> Dynamic t AnswerButtonMode -> m (Event t a)
showAnswerButton a m = answerButton (show a) m a

--answerButtonWChild :: MonadWidget t m => a -> Dynamic t AnswerButtonMode -> m b -> m (Event t a)
--answerButtonWChild a m children = answerButtonWChild' (show a) m a children

revealableButton :: MonadWidget t m => String -> String -> Dynamic t Bool -> m (Event t ())
revealableButton label classWhenVisible isVisible = do
  cssClass <- mapDyn (\x -> bool (classWhenVisible ++ " makeButtonInvisible") classWhenVisible x) isVisible
  buttonDynCss label cssClass

buttonDynCss :: MonadWidget t m => String -> Dynamic t String -> m (Event t ())
buttonDynCss label cssClass =  do
  cssClass' <- mapDyn (singleton "class") cssClass
  (element, _) <- elDynAttr' "button" cssClass' $ text label -- m
  return $ domEvent Click element  -- domEvent :: EventName en -> a -> Event t (EventResultType en)

buttonDiv :: MonadWidget t m => String -> Dynamic t String -> m b -> m(Event t ())
buttonDiv label cssClass children =  do
  cssClass' <- mapDyn (singleton "class") cssClass
  (element, _) <- elDynAttr' "div" cssClass' $ do text label -- m
  children
  return $ domEvent Click element  -- domEvent :: EventName en -> a -> Event t (EventResultType en)

buttonDiv' :: MonadWidget t m => Dynamic t String -> m b -> m(Event t ())
buttonDiv' cssClass children =  do
  cssClass' <- mapDyn (singleton "class") cssClass
  (element, _) <- elDynAttr' "div" cssClass' $ children
  return $ domEvent Click element  -- domEvent :: EventName en -> a -> Event t (EventResultType en)

answerButton :: MonadWidget t m => String -> Dynamic t AnswerButtonMode -> a -> m (Event t a)
answerButton buttonString buttonMode a = do
  c <- mapDyn modeToClass buttonMode
  ev <-  liftM (a <$) $ buttonDynCss buttonString c
  return $ attachWithMaybe f (current buttonMode) ev
  where
    f (NotPossible) _ = Nothing
    f (IncorrectDisactivated) _ = Nothing
    f a b = Just b

answerButtonWChild :: MonadWidget t m => a -> Dynamic t AnswerButtonMode ->  m b -> m (Event t a)
answerButtonWChild a buttonMode children = elClass "div" "clickableDivWrapper" $ do
  c <- mapDyn modeToClassClickableDivButton buttonMode
  ev <-  liftM (a <$) $ buttonDiv' c children
  return $ attachWithMaybe f (current buttonMode) ev
  where
    f (NotPossible) _ = Nothing
    f (IncorrectDisactivated) _ = Nothing
    f a b = Just b


-- makeButton a m = answerButton' m a $ do
-- looks and acts like our answerButton but contains arbitrary children...
-- answerButton' :: MonadWidget t m => Dynamic t AnswerButtonMode -> a -> m b -> m (Event t a)


modeToClass :: AnswerButtonMode -> String
modeToClass NotPossible = "notPossibleButton"
modeToClass Possible = "possibleButton"
modeToClass IncorrectDisactivated = "incorrectDisactivatedButton"
modeToClass Correct = "correctButton"
modeToClass CorrectMissed = "correctButton" -- placeholder: this needs to change to a unique style
modeToClass IncorrectActivated = "incorrectActivatedButton"

modeToClassClickableDivButton :: AnswerButtonMode -> String
modeToClassClickableDivButton NotPossible = "notPossibleButtonClickableDiv"
modeToClassClickableDivButton Possible = "possibleButtonClickableDiv"
modeToClassClickableDivButton IncorrectDisactivated = "incorrectDisactivatedButtonClickableDiv"
modeToClassClickableDivButton Correct = "correctButtonClickableDiv"
modeToClassClickableDivButton CorrectMissed = "correctButtonClickableDiv" -- placeholder: this needs to change to a unique style
modeToClassClickableDivButton IncorrectActivated = "incorrectActivatedButtonClickableDiv"
