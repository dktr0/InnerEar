module InnerEar.Widgets.CreateUser where

import Reflex
import Reflex.Dom
import Control.Monad

import InnerEar.Types.Request
import InnerEar.Types.Response
import InnerEar.Types.Utility

createUserWidget :: MonadWidget t m
  => Event t [Response] -> m (Event t Request,Event t ())
createUserWidget responses = elClass "div" "createUserWidget" $ do

  text "To create a new user, enter an unclaimed handle (i.e. login name) and a password for that user:"

  handleEntry <- elClass "div" "createUserHandle" $ do
    text "Handle:"
    liftM _textInput_value $ textInput def 
  passwordEntry <- elClass "div" "createUserPassword" $ do
    text "Password: "
    liftM _textInput_value $ textInput $ def & textInputConfig_inputType .~ "password"
  createButton <- button "Create New User"
  createValue <- combineDyn CreateUser handleEntry passwordEntry
  let requests = tagDyn createValue createButton

  msgText <- holdDyn "" $ fmap (concat . fmap messageForResponse) responses
  elClass "div" "createUserFeedback" $ dynText msgText

  home <- elClass "div" "navButton" $ button "back to splash page"
  return (requests,home)

messageForResponse :: Response -> String
messageForResponse (UserNotCreated x) = " Unable to create user because: " ++ x
messageForResponse UserCreated = " User successfully created."
messageForResponse _ = ""

