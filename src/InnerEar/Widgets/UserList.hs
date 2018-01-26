module InnerEar.Widgets.UserList where

import Reflex
import Reflex.Dom

import InnerEar.Types.Request
import InnerEar.Types.Response
import Reflex.Synth.Types
import Reflex.Synth.Synth


userListWidget :: MonadWidget t m
  => Event t [Response] -> Dynamic t (Maybe Role) -> m (Event t Request,Event t Sound,Event t ())
userListWidget responses = elClass "div" "excerciseWrapper" $ do
  getUserList <- (GetUserList <$) <$> getPostBuild
  userEvents <- fmapMaybe responseToUser 
  home <- button "Return to splash page"
  let requests = leftmost [getUserList]
  return (requests,never,home)

responseToUser :: Response -> Maybe (String,User)
responseToUser (UserData x@(User h _ _)) = Just (h,x) 
responseToUser _ = Nothing
