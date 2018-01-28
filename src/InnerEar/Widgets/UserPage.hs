module InnerEar.Widgets.UserPage where

import Reflex
import Reflex.Dom
import Data.Maybe
import Data.Map

import InnerEar.Types.Request
import InnerEar.Types.Response
import InnerEar.Types.Data
import InnerEar.Types.User
import InnerEar.Types.Handle
import InnerEar.Widgets.Utility
import Reflex.Synth.Types
import Reflex.Synth.Synth


userPageWidget :: MonadWidget t m
  => Handle -> Event t [Response] -> Dynamic t (Maybe Role) -> m (Event t Request,Event t ())
userPageWidget h responses currentRole = elClass "div" "excerciseWrapper" $ do

  backButton <- divClass "" $ do
    text $ "Data for user " ++ h
    button "Back to SplashPage"

  -- after the widget is built it requests all records on the specified handle
  -- (the server will only respond if there is sufficient permission for this)
  postBuild <- getPostBuild
  let getAllRecords = (GetAllRecords h) <$ postBuild

  -- select any and all server responses that are Records and display them
  let justRecords = fmap (catMaybes . fmap responseToRecord) responses
  recordsList <- foldDyn (++) [] justRecords
  simpleList recordsList $ \r -> do
    divClass "navButton" $ do
      r' <- mapDyn show r
      dynText r'

  return (getAllRecords,backButton)

responseToRecord :: Response -> Maybe Record
responseToRecord (RecordResponse r) = Just r
responseToRecord _ = Nothing
