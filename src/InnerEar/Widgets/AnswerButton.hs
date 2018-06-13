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

-- showableAnswer :: Answer -> String
showableAnswer a = case (show a) of "Answer {frequency = Bass (155 Hz)}" -> "Bass (155 Hz)"
                                    "Answer {frequency = Low Mids (1125 Hz)}" -> "Low Mids (1125 Hz)"
                                    "Answer {frequency = High Mids (3 kHz)}" -> "High Mids (3 kHz)"
                                    "Answer {frequency = Presence (5 kHz)}" -> "Presence (5 kHz)"
                                    "Answer {frequency = Brilliance (13 kHz)}" -> "Brilliance (13 kHz)"
                                    "Answer {frequency = 31}" ->  "31Hz"
                                    "Answer {frequency = 63}" -> "63Hz"
                                    "Answer {frequency = 125}" -> "125Hz"
                                    "Answer {frequency = 250}" -> "250Hz"
                                    "Answer {frequency = 500}" -> "500Hz"
                                    "Answer {frequency = 1k}" ->  "1k"
                                    "Answer {frequency = 2k}" ->  "2k"
                                    "Answer {frequency = 4k}" ->  "4k"
                                    "Answer {frequency = 8k}" ->  "8k"
                                    "Answer {frequency = 16k}" -> "16k"
                                    _ -> show a

showAnswerButton :: (MonadWidget t m, Show a) => a -> Dynamic t AnswerButtonMode -> m (Event t a)
showAnswerButton a m = answerButton (showableAnswer a) m a

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
modeToClassClickableDivButton IncorrectDisactivated = "incorrectDisButtonClickableDiv"
modeToClassClickableDivButton Correct = "correctButtonClickableDiv"
modeToClassClickableDivButton CorrectMissed = "correctMissedButtonClickableDiv" -- placeholder: this needs to change to a unique style
modeToClassClickableDivButton IncorrectActivated = "incorrectActivatedButtonClickableDiv"
