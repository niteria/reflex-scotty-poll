{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
import Web.Scotty hiding (get, delete)
import qualified Web.Scotty as S
import Network.Wai.Middleware.HttpAuth
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Gzip
import System.Environment
import System.IO

import Database.Persist.Sqlite
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as BS

import Auth
import Handlers
import Model

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  runDb $ runMigration migrateAll
  tpl <- Text.decodeUtf8 <$> BS.readFile "index.html.tpl"
  datadata <- Text.decodeUtf8 <$> BS.readFile "data"

  scotty port $ do
    middleware logStdoutDev
    middleware $ gzip def { gzipFiles = GzipCompress }
    middleware $ staticPolicy (noDots >-> addBase "static")
    middleware $ basicAuth checkPassword "Ankieta"
      { authIsProtected = protectedResources }

    S.get "/" (handleIndex datadata tpl)
    S.post "/save" handleSave
    S.get "/admin/results" handleResultsShow
    S.get "/admin/answer/:id" handleAnswerEdit
    S.post "/admin/answer" handleAnswerEditSave
    S.get "/admin/question" (handleQuestionsEditCurrent datadata)
    S.get "/admin/question/:id" handleQuestionsEditId
    S.post "/admin/question" handleQuestionsSave
    S.get "/admin/table" handleResultsTable
    S.get "/admin/tablenormal" handleResultsTableNormal
    S.get "/admin/tablecsv" handleResultsTableCsv
    S.get "/admin/tablenormalcsv" handleResultsTableNormalCsv
    S.get "/admin/result/:id" handleResultShow
    S.post "/admin/resultdelete" handleResultDelete
    S.get "/admin" handleAdminIndex
