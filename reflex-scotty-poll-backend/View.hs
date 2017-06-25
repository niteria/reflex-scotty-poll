{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module View where

import Control.Monad
import Text.Blaze.Html5 hiding (map, main)
import Text.Blaze.Html5.Attributes hiding (headers)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid
import Data.Time (UTCTime)
import Data.Int

questionEditView :: Text -> Html
questionEditView questions = do
  h1 "Edytuj pytanie"
  H.form ! A.action "/admin/question" ! A.method "POST" $ do
    textarea ! A.name "newQuestion" ! A.rows "25" ! A.cols "100" $
      toHtml questions
    H.br
    H.input ! A.type_ "submit" ! A.value "Zapisz"

answerEditView :: Int -> Text -> Text -> Html
answerEditView aid question answer = do
  h1 "Edytuj odpowiedź"
  H.form ! A.action "/admin/answer" ! A.method "POST" $ do
    textarea ! A.name "question" ! A.rows "3" ! A.cols "100" $ toHtml question
    H.br
    textarea ! A.name "answer" ! A.rows "3" ! A.cols "100" $ toHtml answer
    H.input ! A.type_ "hidden" ! A.name "id" ! A.value (toValue aid)
    H.br
    H.input ! A.type_ "submit" ! A.value "Zapisz"

adminIndexView :: Html
adminIndexView = do
  ul $ do
    li $ a ! href "/admin/results" $ "Lista wyników"
    li $ a ! href "/admin/table" $ "Tabela wyników"
    li $ a ! href "/admin/tablenormal" $ "Tabela wyników (pozioma)"
    li $ a ! href "/admin/tablecsv" $ "Tabela wyników (Excel)"
    li $ a ! href "/admin/tablenormalcsv" $ "Tabela wyników (pozioma, Excel)"
    li $ a ! href "/admin/question" $ "Edycja pytania"
    li $ a ! href "/" $ "Ankieta"

resultsTableView :: [[Text]] -> [(Int64, UTCTime, Text, a)] -> Html
resultsTableView v2 allResponses = do
  let thh h = th ! (A.title $ toValue h) $ (toHtml $ Text.take 50 h)
  h1 "Tabela wyników v2"
  table ! customAttribute "border" (toValue $ (1 :: Int)) $ do
    tr $ do
      thh "Wyniki"
      forM_ allResponses $ \(key, created, _, _) ->
        td $ a ! href ("/admin/result/" <> toValue key) $ toHtml $
          Text.pack $ show created
    tr $ do
      thh "Tożsamość"
      forM_ allResponses $ \(_, _, identity, _) ->
        td $ toHtml identity

    forM_ v2 $ \(h:ress) -> tr $ do
      thh h
      forM_ ress $ \res -> do
        td $ toHtml res

resultsTableViewNormal
  :: [Text] -> [[Text]] -> [(Int64, UTCTime, Text, a)] -> Html
resultsTableViewNormal headers texts allResponses = do
  let thh h = th ! (A.title $ toValue h) $ (toHtml $ Text.take 50 h)
  h1 "Tabela wyników pozioma"
  table ! customAttribute "border" (toValue $ (1 :: Int)) $ do
    tr $ do
      thh "Wyniki"
      thh "Tożsamość"
      forM_ headers $ \h -> thh h
    forM_ (zip texts allResponses) $ \(ts, (key, created, identity, _)) -> do
      tr $ do
        td $ a ! href ("/admin/result/" <> toValue key) $ toHtml $
          Text.pack $ show created
        td $ toHtml identity
        forM_ ts $ \t -> do
          td $ toHtml t
