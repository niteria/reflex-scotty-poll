{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

import Reflex.Dom
import Control.Monad.Fix
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Types
import FFI
import Reflex.Dom.Contrib.Xhr
import qualified Data.List.NonEmpty as NonEmpty

main :: IO ()
main = do
  initialNode <- initialQuestion
  mainWidget (currentQuestionW' initialNode)

currentQuestionW'
  :: ( DomBuilder t m
     , MonadWidget t m
     )
  => QNode
  -> m ()
currentQuestionW' initialNode = do
  rec currentNode <- foldDyn ($) initialNode $ mergeWith (.)
                       [ setNewNode
                       ]
      nodeSelected <- nodeRender currentNode
      elDynAttr "img" (bgImage' <$> currentNode) $ return ()
      let setNewNode = fmap (const . nextNode) nodeSelected
      let answersTuple = Map.singleton ("answers" :: Text) .
            map (\Answer{..} -> (question, answer))
      history <- foldDyn (:) [] nodeSelected
      let lastNodeSelected = ffilter headLast (updated history)
      let headLast [] = False
          headLast (a:_) = null $ choices $ nextNode a
      _ <- performJsonAjax $ fmap (("save",) . answersTuple)
        (fmap reverse lastNodeSelected)
  return ()

bgImage :: QNode -> Map Text Text
bgImage QNode{..} = Map.fromList
  [ ("style", "background-image: url('" <> background <> "');")
  , ("id", "all")
  ]

bgImage' :: QNode -> Map Text Text
bgImage' QNode{..} = Map.fromList
  [ ("src", background)
  , ("id", "bg")
  ]

qImage :: QNode -> Map Text Text
qImage QNode{..} = Map.fromList
  [ ("src", picture)
  ]

preQImage :: QNode -> Map Text Text
preQImage QNode{..} = Map.fromList
  [ ("src", picture)
  , ("style", "display:none")
  ]

preBgImage' :: QNode -> Map Text Text
preBgImage' QNode{..} = Map.fromList
  [ ("src", background)
  , ("style", "display:none")
  ]

nodeRender
  :: forall t m .
     ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t QNode
  -> m (Event t Answer)
nodeRender qNode = do
  elAttr "div" ("class" =: "question") $ do
    elDynAttr "img" (qImage <$> qNode) $ return ()
    -- elDynHtml' "p" (intro <$> qNode)
    _ <- elAttr "div" ("class" =: "msg") $
      simpleList (Text.splitOn "|||" . intro <$> qNode) $ \p ->
        el "p" $ dynText p
      -- dynText (intro <$> qNode)
    (possibleAnswers :: Dynamic t [Event t Answer]) <- do
      simpleList (choices <$> qNode) (answerButton qNode)
    let combinePossibleAnswers :: [Event t Answer] -> Event t Answer
        combinePossibleAnswers = fmap NonEmpty.head . mergeList
        possibleAnswerEvent = fmap combinePossibleAnswers possibleAnswers
        possibleAnswerEvent :: Dynamic t (Event t Answer)
        possibleNodes :: Event t Answer
        possibleNodes = switch $ current possibleAnswerEvent
    return possibleNodes

answerButton
  :: forall t m . (DomBuilder t m, PostBuild t m)
  => Dynamic t QNode
  -> Dynamic t Choice
  -> m (Event t Answer)
answerButton currentNode choice = do
  let nextNode = next <$> choice
      nextNode :: Dynamic t QNode
      answerText = msg <$> choice

  ev <- dynButton answerText
  elDynAttr "img" (preBgImage' <$> nextNode) $ return ()
  elDynAttr "img" (preQImage <$> nextNode) $ return ()
  return $ fmap (\(a, (q, n)) -> Answer q a n) $ attach (current answerText) $
    attach (current (intro <$> currentNode)) $ tag (current nextNode) ev

dynButton
  :: forall t m . (PostBuild t m, DomBuilder t m)
  => Dynamic t Text
  -> m (Event t ())
dynButton s = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: "answer") $
    dynText s
  return $ domEvent Click e
