{-# LANGUAGE OverloadedStrings, TupleSections #-}

module InnerEar.Database.Events where

import Text.Read
import Data.Either
import Data.Maybe
import Data.Text
import Data.Char
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok

import InnerEar.Types.ExerciseId
import InnerEar.Types.Handle
import InnerEar.Types.Data
import InnerEar.Types.ExerciseId

-- definitions pertaining to "events", ie. the type Record and all of its dependencies

createEventsTable :: Connection -> IO ()
createEventsTable c =
  execute_ c "CREATE TABLE IF NOT EXISTS events (handle TEXT, time TEXT, event TEXT, exerciseId TEXT, config TEXT, question TEXT, answer TEXT, selection TEXT, shortTermEval TEXT, longTermEval TEXT, reflection TEXT)"

instance FromField ExerciseId where
  fromField = f . readMaybe . g . fieldData
    where g (SQLText t) = unpack t
          g _ = ""
          f (Just x) = Ok x
    -- *** incomplete pattern matching here is a bad bad bad temporary idea...

instance ToField ExerciseId where
  toField = SQLText . pack . show

instance FromRow Record where
  fromRow = do
    h <- field
    t <- field
    (e,i,c,q,a,s,e1,e2,r) <- fromRow
    let x = recordFromRow h t e i c q a s e1 e2 r
    let uhoh = fail "unable to parse Record in FromRow instance"
    maybe uhoh return x

recordFromRow :: Maybe String -> Maybe Time -> Maybe String -> Maybe ExerciseId -> Maybe String
  -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String
  -> Maybe String -> Maybe Record

recordFromRow h t (Just "Started") i c q a s e1 e2 r = do
  y <- (Left . (,ExerciseStarted)) <$> i
  z <- Point y <$> t
  (flip Record) z <$> h

recordFromRow h t (Just "Configured") i c q a s e1 e2 r = do
  x <- ExerciseConfigured <$> c
  y <- (Left . (,x)) <$> i
  z <- Point y <$> t
  (flip Record) z <$> h

recordFromRow h t (Just "NewQuestion") i c q a s e1 e2 r = do
  x <- ExerciseNewQuestion <$> c <*> q <*> a
  y <- (Left . (,x)) <$> i
  z <- Point y <$> t
  (flip Record) z <$> h

recordFromRow h t (Just "ListenedQuestion") i c q a s e1 e2 r = do
  x <- ExerciseListenedQuestion <$> c <*> q <*> a
  y <- (Left . (,x)) <$> i
  z <- Point y <$> t
  (flip Record) z <$> h

recordFromRow h t (Just "ListenedReference") i c q a s e1 e2 r = do
  x <- ExerciseListenedReference <$> c <*> q <*> a
  y <- (Left . (,x)) <$> i
  z <- Point y <$> t
  (flip Record) z <$> h

recordFromRow h t (Just "Answered") i c q a s e1 e2 r = do
  x <- ExerciseAnswered <$> s <*> e1 <*> e2 <*> c <*> q <*> a
  y <- (Left . (,x)) <$> i
  z <- Point y <$> t
  (flip Record) z <$> h

recordFromRow h t (Just "ListenedExplore") i c q a s e1 e2 r = do
  x <- ExerciseListenedExplore <$> s <*> c <*> q <*> a
  y <- (Left . (,x)) <$> i
  z <- Point y <$> t
  (flip Record) z <$> h

recordFromRow h t (Just "Reflection") i c q a s e1 e2 r = do
  x <- ExerciseReflection <$> r
  y <- (Left . (,x)) <$> i
  z <- Point y <$> t
  (flip Record) z <$> h

recordFromRow h t (Just "Store") i c q a s e1 e2 r = do
  x <- ExerciseStore <$> c -- Note: somewhat arbitrarily/temporarily we are placing Store data in the config column of the database table...
  y <- (Left . (,x)) <$> i
  z <- Point y <$> t
  (flip Record) z <$> h

recordFromRow h t (Just "Ended") i c q a s e1 e2 r = do
  y <- (Left . (,ExerciseEnded)) <$> i
  z <- Point y <$> t
  (flip Record) z <$> h

recordFromRow h t (Just "SessionStart") i c q a s e1 e2 r = do
  let y = Right SessionStart
  z <- Point y <$> t
  (flip Record) z <$> h

recordFromRow h t (Just "SessionEnd") i c q a s e1 e2 r = do
  let y = Right SessionEnd
  z <- Point y <$> t
  (flip Record) z <$> h

recordFromRow h t (Just "AuthenticationFailure") i c q a s e1 e2 r = do
  let y = Right AuthenticationFailure
  z <- Point y <$> t
  (flip Record) z <$> h

recordFromRow _ _ _ _ _ _ _ _ _ _ _ = Nothing

nil :: SQLData
nil = toField (Nothing :: Maybe Text)

instance
  (ToField a,ToField b,ToField c, ToField d,ToField e,ToField f,
   ToField g, ToField h, ToField i, ToField j, ToField k) =>
  ToRow (a,b,c,d,e,f,g,h,i,j,k) where
  toRow (a,b,c,d,e,f,g,h,i,j,k) =
    [toField a,toField b,toField c,toField d,toField e,toField f,toField g,toField h,
     toField i,toField j,toField k]

instance ToRow Record where
  toRow (Record h (Point (Left (i,ExerciseStarted)) t)) =
    toRow (h,t,"Started"::Text,i,nil,nil,nil,nil,nil,nil,nil)
  toRow (Record h (Point (Left (i,ExerciseConfigured c)) t)) =
    toRow (h,t,"Configured"::Text,i,c,nil,nil,nil,nil,nil,nil)
  toRow (Record h (Point (Left (i,ExerciseNewQuestion c q a)) t)) =
    toRow (h,t,"NewQuestion"::Text,i,c,q,a,nil,nil,nil,nil)
  toRow (Record h (Point (Left (i,ExerciseListenedQuestion c q a)) t)) =
    toRow (h,t,"ListenedQuestion"::Text,i,c,q,a,nil,nil,nil,nil)
  toRow (Record h (Point (Left (i,ExerciseListenedReference c q a)) t)) =
    toRow (h,t,"ListenedReference"::Text,i,c,q,a,nil,nil,nil,nil)
  toRow (Record h (Point (Left (i,ExerciseAnswered ia e1 e2 c q a)) t)) =
    toRow (h,t,"Answered"::Text,i,c,q,a,ia,e1,e2,nil)
  toRow (Record h (Point (Left (i,ExerciseListenedExplore s c q a)) t)) =
    toRow (h,t,"ListenedExplore"::Text,i,c,q,a,s,nil,nil,nil)
  toRow (Record h (Point (Left (i,ExerciseReflection r)) t)) =
    toRow (h,t,"Reflection"::Text,i,nil,nil,nil,nil,nil,nil,r)
  toRow (Record h (Point (Left (i,ExerciseStore s)) t)) =
    toRow (h,t,"Store"::Text,i,s,nil,nil,nil,nil,nil,nil)
  toRow (Record h (Point (Left (i,ExerciseEnded)) t)) =
    toRow (h,t,"Ended"::Text,i,nil,nil,nil,nil,nil,nil,nil)
  toRow (Record h (Point (Right SessionStart) t)) =
    toRow (h,t,"SessionStart"::Text,nil,nil,nil,nil,nil,nil,nil,nil)
  toRow (Record h (Point (Right SessionEnd) t)) =
    toRow (h,t,"SessionEnd"::Text,nil,nil,nil,nil,nil,nil,nil,nil)
  toRow (Record h (Point (Right AuthenticationFailure) t)) =
    toRow (h,t,"AuthenticationFailure"::Text,nil,nil,nil,nil,nil,nil,nil,nil)

postEvent :: Connection -> Record -> IO ()
postEvent c r = execute c "INSERT INTO events (handle,time,event,exerciseId,config,question,answer,selection,shortTermEval,longTermEval,reflection) VALUES (?,?,?,?,?,?,?,?,?,?,?)" r

findAllRecords :: Connection -> Handle -> IO [Record]
findAllRecords conn h = query conn "SELECT handle,time,event,exerciseId,config,question,answer,selection,shortTermEval,longTermEval,reflection FROM events WHERE handle = ?" (Only (fmap Data.Char.toLower h))

findAllExerciseEvents :: Connection -> Handle -> ExerciseId -> IO [Record]
findAllExerciseEvents conn h e = query conn "SELECT handle,time,event,exerciseId,config,question,answer,selection,shortTermEval,longTermEval,reflection FROM events WHERE handle = ? AND exerciseId = ?" (fmap Data.Char.toLower h,show e)
