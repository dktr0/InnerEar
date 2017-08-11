module InnerEar.Widgets.CreateUser where

import Reflex
import Reflex.Dom

import InnerEar.Types.Request
import InnerEar.Types.Response
import InnerEar.Types.Utility

createUserWidget :: MonadWidget t m
  => Event t [Response] -> m (Event t Request,Event t ())
createUserWidget responses = el "div" $ do

  text "To create a new user, enter an unclaimed handle (i.e. login name) and a password for that user:"

  handleEntry <- do
    text "Handle:"
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  passwordEntry <- do
    text "Password: "
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_inputType .~ "password" & textInputConfig_attributes .~ attrs
  createButton <- button "Create New User"
  createValue <- combineDyn CreateUser handleEntry passwordEntry
  let requests = tagDyn createValue createButton

  -- when they fail to create a new user, display a message to that effect
  let failEvent = fmapMaybe $ lastWithPredicate (==UserNotCreated) responses
  failText <- holdDyn "" $ fmap (const " Unable to create user!") failEvent
  dynText failText

  -- go back to splash page if they press the back button, or if we succeed in creating a new user
  home <- button "back to splash page"
  let successEvent = fmapMaybe $ lastWithPredicate isAuthenticated responses
  let back = leftmost [home,successEvent]
  return (requests,back)
