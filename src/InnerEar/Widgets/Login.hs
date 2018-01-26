{-# LANGUAGE RecursiveDo #-}

module InnerEar.Widgets.Login where

import Reflex
import Reflex.Dom
import Control.Monad

import InnerEar.Types.Request
import InnerEar.Types.Response
import InnerEar.Types.Handle
import InnerEar.Types.Utility
import InnerEar.Types.User

-- | A loginWidget appears as a single line in the interface. It displays a
-- set of text fields and button if the user is not logged in. If the user
-- is logged in it displays that, as well as a button to log out.

-- needs to only rebuild when status changes!!!

loginWidget :: MonadWidget t m
  => Event t [Response] -> m (Event t Request, Dynamic t (Maybe Role))
loginWidget responses = elClass "div" "loginWidget" $ mdo
  let initialWidget = notLoggedInWidget responses
  let p x = ( x == NotAuthenticated || isAuthenticated x)
  let newStatus = fmapMaybe (lastWithPredicate p) responses --
  status <- updated <$> nubDyn <$> holdDyn NotAuthenticated newStatus
  let authEvents = fmapMaybe getHandleFromAuthenticated $ ffilter (isAuthenticated) status
  let buildLoggedIn = fmap (loggedInWidget responses) authEvents
  -- let notAuthEvents = ffilter (==NotAuthenticated) status
  let buildTryToLogIn = fmap (const $ tryToLoginWidget responses) goToLogin
  let deAuthEvents = ffilter (==Deauthenticate) r
  let buildNotLoggedIn = fmap (const $ notLoggedInWidget responses) deAuthEvents
  let rebuildEvents = leftmost [buildLoggedIn,buildNotLoggedIn,buildTryToLogIn]
  x <- widgetHold initialWidget rebuildEvents
  r <- switchPromptlyDyn <$> mapDyn fst x
  goToLogin <- switchPromptlyDyn <$> mapDyn snd x
  let newRole = fmap getRoleFromResponse newStatus
  currentRole <- holdDyn Nothing newRole
  -- areTheyAuthenticated <- holdDyn NotAuthenticated newStatus >>= mapDyn (not . (==NotAuthenticated))
  return (r,currentRole)

getRoleFromResponse :: Response -> Maybe Role
getRoleFromResponse (Authenticated _ r) = Just r
getRoleFromResponse (NotAuthenticated) = Nothing
getRoleFromResponse _ = error "getRoleFromResponse shouldn't be called with anything other than Authenticated or NotAuthenticated"

tryToLoginWidget :: MonadWidget t m => Event t [Response] -> m (Event t Request,Event t ())
tryToLoginWidget responses = do
  handleEntry <- do
    text "Username:"
    _textInput_value <$> (textInput $ def)
  passwordEntry <- do
    text "Password: "
    _textInput_value <$> (textInput $ def & textInputConfig_inputType .~ "password")
  loginButton <- button "Login"
  -- when they fail to authenticate, display a message to that effect
  let deauthEvent = fmapMaybe (lastWithPredicate (==NotAuthenticated)) responses
  deauthText <- holdDyn "" $ fmap (const " Incorrect login!") deauthEvent
  dynText deauthText
  loginValue <- combineDyn Authenticate handleEntry passwordEntry
  return (tagDyn loginValue loginButton,never)


loggedInWidget :: MonadWidget t m => Event t [Response] -> Handle -> m (Event t Request,Event t ())
loggedInWidget _ h = do
  text $ "Logged in as " ++ h
  r <- (Deauthenticate <$) <$> button "Logout"
  return (r,never)

notLoggedInWidget :: MonadWidget t m => Event t [Response] -> m (Event t Request,Event t ())
notLoggedInWidget _ = do
  r <- button "Login"
  return (never,r)
