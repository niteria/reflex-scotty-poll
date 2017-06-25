{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model where

import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.TH
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger
import Control.Monad
import Database.Persist.Sqlite
import Database.Persist.Postgresql
import Web.Heroku (dbConnParams)
import Data.Monoid ((<>))
import System.Environment
import qualified Data.Text.Encoding as Text
import Control.Monad.IO.Class
import Data.Int

runDbDev :: SqlPersistM a -> IO a
runDbDev query = runResourceT . runNoLoggingT .
  withSqliteConn "dev.sqlite3" . runSqlConn $ query

runDbHeroku :: SqlPersistM a -> IO a
runDbHeroku query = do
    params <- dbConnParams
    let connStr = foldr (\(k,v) t ->
          t <> (Text.encodeUtf8 $ k <> "=" <> v <> " ")) "" params
    runResourceT . runNoLoggingT . withPostgresqlConn connStr $ runSqlConn query

runDb :: SqlPersistM a -> IO a
runDb q = do
  dbURL <- lookupEnv "DATABASE_URL"
  case dbURL of
    Nothing -> runDbDev q
    _ -> runDbHeroku q

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Result
    createdAt UTCTime
    identity Text default=''
    deriving Show
Answer
    pollResultId ResultId
    question Text
    answer Text
    deriving Show
Questions
    blob Text
    createdAt UTCTime
|]

db :: MonadIO m => SqlPersistM a -> m a
db = liftIO . runDb

currentQuestions :: SqlPersistM (Maybe (Entity Questions))
currentQuestions = do
  r <- selectList [] [LimitTo 1, Desc QuestionsCreatedAt]
  case r of
    [] -> return Nothing
    (q:_) -> return (Just q)

saveAnswers :: MonadIO m => Text -> [(Text, Text)] -> m ()
saveAnswers identity answers = do
  now <- liftIO getCurrentTime
  db $ do
    resultId <- insert $ Result now identity
    forM_ answers $ \(question, answer) -> do
      _ <- insert $ Answer resultId question answer
      return ()

deleteResult :: Key Result -> SqlPersistM ()
deleteResult key = do
  deleteWhere [AnswerPollResultId ==. key]
  delete key

getAllResponses :: SqlPersistM [(Int64, UTCTime, Text, [Answer])]
getAllResponses = do
  results :: [Entity Result] <- selectList [] [Asc ResultId]
  forM results $ \(Entity key Result{..}) -> do
    answers :: [Answer] <-
      map entityVal <$> selectList [AnswerPollResultId ==. key] [Asc AnswerId]
    return (fromSqlKey key, resultCreatedAt, resultIdentity, answers)
