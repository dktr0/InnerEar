module InnerEar.Widgets.UserList where

import Reflex
import Reflex.Dom
import Data.Maybe

import InnerEar.Types.Request
import InnerEar.Types.Response
import InnerEar.Types.User
import InnerEar.Types.Handle
import Reflex.Synth.Types
import Reflex.Synth.Synth


userListWidget :: MonadWidget t m
  => Event t [Response] -> Dynamic t (Maybe Role) -> m (Event t Request,Event t (Maybe Handle))
userListWidget responses role = elClass "div" "excerciseWrapper" $ do

  -- after the widget is built it requests info on all users from the server, but only if role is Administrator
  postBuild <- getPostBuild
  isAdministrator <- mapDyn (== (Just Administrator)) role
  let getUserList = GetUserList <$ gate (current isAdministrator) postBuild

  -- select any and all server responses that are UserData to display a clickable map of all users
  let userEvents = fmap (catMaybes . fmap responseToUser) responses

{-
  let deltasDown' = fmap justEnsembleResponses deltasDown
    let spaceAndDeltasDown = attachDyn currentSpace deltasDown'
    let justInSpace = fmap (\(x,y) -> justSited x $ y) spaceAndDeltasDown
    let responseMsgs = fmap (mapMaybe messageForEnsembleResponse) justInSpace
    let messages = mergeWith (++) [responseMsgs,errorMsgs]
    mostRecent <- foldDyn (\a b -> take 12 $ (reverse a) ++ b) [] messages
  simpleList mostRecent $ \v -> divClass "chatMessage" $ dynText v
-}

  -- widget asks to be closed when back button is pressed, or anytime role is not Administrator
  backButton <- (Nothing <$) <$> button "Back"
  let notAdminPostBuild = (Nothing <$) $ ffilter (/= (Just Administrator)) $ tagDyn role postBuild
  let notAdminLater = (Nothing <$) $ ffilter (/= (Just Administrator)) $ updated role
  let nav = leftmost [backButton,notAdminPostBuild,notAdminLater]
  return (getUserList,nav)

responseToUser :: Response -> Maybe (String,User)
responseToUser (UserData x@(User h _ _)) = Just (h,x)
responseToUser _ = Nothing
