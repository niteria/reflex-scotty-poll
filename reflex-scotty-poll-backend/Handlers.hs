{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Handlers
  ( handleIndex
  , handleSave

  , handleAdminIndex

  , handleResultShow
  , handleResultDelete
  , handleResultsShow
  , handleResultsTable
  , handleResultsTableCsv
  , handleResultsTableNormal
  , handleResultsTableNormalCsv

  , handleAnswerEdit
  , handleAnswerEditSave

  , handleQuestionsSave
  , handleQuestionsEditId
  , handleQuestionsEditCurrent
  ) where

import Web.Scotty hiding (get, delete, headers)
import Web.Scotty.Cookie
import qualified Web.Scotty as S
import Control.Monad
import Model
import Control.Monad.IO.Class
import Data.Monoid
import Text.Blaze.Html5 hiding (map, main)
import Text.Blaze.Html5.Attributes hiding (headers)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text

import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Sqlite
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LT
import Data.Text (Text)
import Control.Arrow ((&&&))
import Data.List (transpose)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Graph
import Data.Word
import Data.Char
import Data.Maybe
import System.Random (randomIO)

import View

-- TODO: move some stuff to View or Model
-- TODO: multiple language support

blaze :: Html -> ActionM ()
blaze = S.html . renderHtml

handleIndex :: Text -> Text -> ActionM ()
handleIndex datadata tpl = do
  mbQuestions <- db currentQuestions
  let questions = maybe datadata (questionsBlob . entityVal) mbQuestions
  let indexContents = Text.replace "DATADATA" questions tpl
  S.html (LT.fromStrict indexContents)

handleSave :: ActionM ()
handleSave = do
  answersRaw :: ByteString <- S.param "answers"
  let identCookieKey = "K3yH4dvNLebH"
  mbIdentCookie <- getCookie identCookieKey
  identityRand <-
    case mbIdentCookie of
      Just identity -> return identity
      Nothing -> do
        rand <- Text.pack . show <$> liftIO (randomIO :: IO Word64)
        setSimpleCookie identCookieKey rand
        return rand
  identityIP <- fromMaybe "No X-Forwarded-For" <$> S.header "X-Forwarded-For"
  answers :: [(Text, Text)] <-
    case eitherDecode answersRaw of
      Right answers -> return answers
      Left err -> do
        liftIO $ putStrLn err
        raise "500"
  let identity =
        Text.map sanitize (identityRand <> " " <> LT.toStrict identityIP)
      sanitize c
        | isAscii c && isPrint c && (c /= ';') = c
        | otherwise = '_'
  saveAnswers identity answers
  blaze "OK"

handleAdminIndex :: ActionM ()
handleAdminIndex = blaze adminIndexView

handleResultDelete :: ActionM ()
handleResultDelete = do
  key :: Key Result <- toSqlKey <$> S.param "id"
  db $ deleteResult key
  redirect "/admin/results"

handleAnswerEdit :: ActionM ()
handleAnswerEdit = do
  key <- toSqlKey <$> S.param "id"
  (Just Answer{..}) <- db (get key)
  blaze $ answerEditView (fromIntegral $ fromSqlKey key) answerQuestion answerAnswer

handleAnswerEditSave :: ActionM ()
handleAnswerEditSave = do
  key <- toSqlKey <$> S.param "id"
  question <- S.param "question"
  answer <- S.param "answer"
  db $ update key [AnswerQuestion =. question, AnswerAnswer =. answer]
  (Just Answer{..}) <- db (get key)
  redirect ("/admin/result/" <> LT.pack (show $ fromSqlKey answerPollResultId))

handleResultShow :: ActionM ()
handleResultShow = do
  key <- toSqlKey <$> S.param "id"
  (Just Result{..}) <- db (get key)
  answers :: [Entity Answer]
    <- db $ selectList [AnswerPollResultId ==. key] [Asc AnswerId]
  blaze $ do
    a ! href ("/admin/results") $ "Wroć"
    h6 (toHtml $ show resultCreatedAt)
    h6 (toHtml resultIdentity)
    h1 "Odpowiedzi:"
    ul $ do
      forM_ answers $ \(Entity akey Answer{..}) -> do
        li $ do
          toHtml answerQuestion
          ul $ do
            li (toHtml answerAnswer)
            li $ a ! href ("/admin/answer/" <> toValueKey akey) $ "Edytuj"
    H.form ! A.action "/admin/resultdelete" ! A.method "POST" $ do
      H.input ! A.type_ "hidden" ! A.name "id" ! A.value (toValueKey key)
      H.input ! A.type_ "submit" ! A.value "Bezpowrotnie usuń"

computeTable :: [(a, b, c, [Answer])] -> [[Text]]
computeTable allResponses =
  transpose (let (h, ts) = computeTableNormal allResponses in h:ts)

computeTableNormal :: [(a, b, c, [Answer])] -> ([Text], [[Text]])
computeTableNormal allResponses = (headersText, rTable)
  where
  order = Map.fromListWith Set.union
    ([ (q1, Set.singleton q2)
    | (_, _, _, answers) <- allResponses
    , (Answer { answerQuestion = q1 }, Answer { answerQuestion = q2 }) <-
        zip answers (drop 1 answers)
    ] ++
    [ (q1, Set.empty)
    | (_, _, _, answers) <- allResponses
    , Answer { answerQuestion = q1 } <- answers
    ])
  topo = reverse $ flattenSCCs $
    stronglyConnComp [ (v, v, Set.toList ess) | (v, ess) <- Map.toList order ]
  questionsAnswers = Map.fromListWith Set.union
    [ (question, Set.singleton answer)
    | (_, _, _, answers) <- allResponses
    , Answer { answerQuestion = question, answerAnswer = answer } <- answers
    ]
  qaHeaders = concat
              [ (question, Nothing):zip (repeat question) answers
              | question <- topo
              , let answers = fmap Just $ Set.toList $
                      Map.findWithDefault Set.empty question questionsAnswers
              ]
  headersText = map toHeaderText qaHeaders
  toHeaderText (question, Nothing) = question
  toHeaderText (_, Just answer) = answer
  rTable = [ map (forHeader qToA) qaHeaders
            | (_, _created, _identity, answers) <- allResponses
            , let qToA = Map.fromList $
                            map (answerQuestion &&& answerAnswer) answers
            ]
  forHeader _qToA (_, Nothing) = "-"
  forHeader qToA (question, Just answer) =
    case Map.lookup question qToA of
      Just answer' -> if answer' == answer then answer else "-"
      Nothing -> "-"

handleResultsTable :: ActionM ()
handleResultsTable = do
  allResponses <- db getAllResponses
  let v2 = computeTable allResponses
  blaze $ resultsTableView v2 allResponses

handleResultsTableNormal :: ActionM ()
handleResultsTableNormal = do
  allResponses <- db getAllResponses
  let (headers,texts) = computeTableNormal allResponses
  blaze $ resultsTableViewNormal headers texts allResponses

handleResultsTableCsv :: ActionM ()
handleResultsTableCsv = do
  setHeader "Content-Type" "text/csv; charset=utf-8"
  setHeader "Content-Disposition" "attachment; filename=ankieta.csv"
  allResponses <- db getAllResponses
  let v2 = computeTable allResponses
  let hd = "Wyniki":[ Text.pack (show created)
                    | (_, created, _, _) <- allResponses ]
  let hd2 = "Tożsamość":[ identity | (_, _, identity, _) <- allResponses ]
  raw $ BSL.fromStrict $ Text.encodeUtf8 $
    Text.unlines $ map toCSVLine (hd:hd2:v2)

toCSVLine :: [Text] -> Text
toCSVLine = Text.intercalate ";"

handleResultsTableNormalCsv :: ActionM ()
handleResultsTableNormalCsv = do
  setHeader "Content-Type" "text/csv; charset=utf-8"
  setHeader "Content-Disposition" "attachment; filename=ankieta.csv"
  allResponses <- db getAllResponses
  let (headers,texts) = computeTableNormal allResponses
  raw $ BSL.fromStrict $ Text.encodeUtf8 $ Text.unlines $ map toCSVLine
    (("Wyniki":"Tożsamość":headers):
     [ Text.pack (show created):identity:ts
     | (ts, (_, created, identity,_)) <-  zip texts allResponses
     ])

toValueKey :: ToBackendKey SqlBackend a => Key a -> AttributeValue
toValueKey = toValue . fromSqlKey

handleQuestionsSave :: ActionM ()
handleQuestionsSave = do
  newQuestion :: Text <- S.param "newQuestion"
  now <- liftIO getCurrentTime
  key <- db $ insert $ Questions newQuestion now
  redirect ("/admin/question/" <> LT.pack (show $ fromSqlKey key))

handleQuestionsEditId :: ActionM ()
handleQuestionsEditId = do
  key <- toSqlKey <$> S.param "id"
  mbQuestions :: Maybe (Questions) <- db $ get key
  questions <- maybe next (return . questionsBlob) mbQuestions
  blaze $ questionEditView questions

handleQuestionsEditCurrent :: Text -> ActionM ()
handleQuestionsEditCurrent datadata = do
  mbQuestions <- db currentQuestions
  let questions = maybe datadata (questionsBlob . entityVal) mbQuestions
  blaze $ questionEditView questions

handleResultsShow :: ActionM ()
handleResultsShow = do
  results :: [Entity Result] <- (liftIO $ runDb $ selectList [] [Asc ResultId])
  blaze $ do
    h1 "Lista wyników"
    ol $ do
      forM_ results $ \(Entity key Result{..}) -> do
        li $
          a ! href ("/admin/result/" <> toValue (fromSqlKey key)) $ toHtml $
            Text.pack $ show resultCreatedAt
