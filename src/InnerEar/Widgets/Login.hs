module InnerEar.Widgets.Login where

import Reflex
import Reflex.Dom
import Control.Monad

import InnerEar.Types.Request
import InnerEar.Types.Response
import InnerEar.Types.Handle

-- | A loginWidget appears as a single line in the interface. It displays a
-- set of text fields and button if the user is not logged in. If the user
-- is logged in it displays that, as well as a button to log out.

loginWidget :: MonadWidget t m
  => Event t Response -> m (Event t Request)
loginWidget responses = el "div" $ do
  let initialWidget = notLoggedInWidget responses
  let authEvents = fmapMaybe getHandleFromAuthenticated responses
  let buildLoggedIn = fmap (loggedInWidget responses) authEvents
  let deauthEvents = ffilter (==NotAuthenticated) responses
  let buildNotLoggedIn = fmap (const $ notLoggedInWidget responses) deauthEvents
  let rebuildEvents = leftmost [buildLoggedIn,buildNotLoggedIn]
  liftM switchPromptlyDyn $ widgetHold initialWidget rebuildEvents


notLoggedInWidget :: MonadWidget t m => Event t Response -> m (Event t Request)
notLoggedInWidget responses = el "div" $ do
  handleEntry <- do
    text "Handle:"
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  passwordEntry <- do
    text "Password: "
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_inputType .~ "password" & textInputConfig_attributes .~ attrs
  loginButton <- button "Login"
  -- when they fail to authenticate, display a message to that effect
  let deauthEvent = ffilter (==NotAuthenticated) responses
  deauthText <- holdDyn "" $ fmap (const " Incorrect login!") deauthEvent
  dynText deauthText
  loginValue <- combineDyn Authenticate handleEntry passwordEntry
  return $ tagDyn loginValue loginButton


loggedInWidget :: MonadWidget t m => Event t Response -> Handle -> m (Event t Request)
loggedInWidget _ h = el "div" $ do
  text $ "Logged in as " ++ h
  liftM (Deauthenticate <$) $ button "Logout"
