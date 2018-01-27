module InnerEar.Widgets.UserList where

import Reflex
import Reflex.Dom
import Data.Maybe

import InnerEar.Types.Request
import InnerEar.Types.Response
import InnerEar.Types.User
import Reflex.Synth.Types
import Reflex.Synth.Synth


userListWidget :: MonadWidget t m
  => Event t [Response] -> Dynamic t (Maybe Role) -> m (Event t Request,Event t Sound,Event t ())
userListWidget responses role = elClass "div" "excerciseWrapper" $ do
  postBuild <- getPostBuild
  let getUserList = GetUserList <$ postBuild
  let userEvents = fmap (catMaybes . fmap responseToUser) responses

  let requests = leftmost [getUserList]
  -- widget asks to be closed when back button is pressed, or anytime role is not Administrator
  backButton <- button "Back"
  let notAdminPostBuild = (() <$) $ ffilter (/= (Just Administrator)) $ tagDyn role postBuild
  let notAdminLater = (() <$) $ ffilter (/= (Just Administrator)) $ updated role
  let back = leftmost [backButton,notAdminPostBuild,notAdminLater]
  return (requests,never,back)

responseToUser :: Response -> Maybe (String,User)
responseToUser (UserData x@(User h _ _)) = Just (h,x)
responseToUser _ = Nothing
