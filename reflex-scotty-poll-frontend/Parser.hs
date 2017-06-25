{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Parser
  ( Node(..)
  , Choice(..)
  , parseNodes
  , serializeNodes
  ) where

import qualified Types
import Types (QNode)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import GHC.Generics
import Data.Text (Text)
import Data.List (group, sort)
import qualified Data.Text as Text
import Data.Maybe
import Text.Show.Unicode

data Node = Node
  { name :: Text
  , intro :: Text
  , background :: Text
  , picture :: Text
  , choices :: [Choice]
  } deriving (Generic, Show, Read)

data Choice = Choice
  { msg :: Text
  , next :: Text
  } deriving (Generic, Show, Read)

parseNodes :: String -> Either String QNode
parseNodes bs = do
  unresolvedNodes :: [Node] <-
    case reads bs of
      [(res, "")] -> return res
      r -> Left $ "No parse: " ++ show (r, bs)
  checkDuplicates unresolvedNodes
  checkStart unresolvedNodes
  let names = Set.fromList $ map name unresolvedNodes
  checkNext unresolvedNodes names
  let resolvedNodes = Map.fromList $ map resolveNode unresolvedNodes
      resolveNode Node{..} =
        (name, Types.QNode{choices = (map resolveChoice choices), ..})
      resolveChoice (Choice msg next) =
        Types.Choice msg (fromJust $ Map.lookup next resolvedNodes)
  return (fromJust $ Map.lookup "start" resolvedNodes)
  where
  checkDuplicates ns =
    case filter ((>1) . length) $ group $ sort $ map name ns of
      [] -> return ()
      dups -> Left $ "Duplicate nodes: " ++
        Text.unpack (Text.intercalate ", " $ map head dups)
  checkStart ns =
    if any ((== "start") . name) ns
      then return ()
      else Left "No node named 'start'"
  checkNext ns names =
    case invalids of
      [] -> return ()
      invalid -> Left $ "Invalid next nodes: " ++
        Text.unpack (Text.intercalate ", " invalid)
    where
    invalids = filter (not . (`Set.member` names)) $
      concatMap (map next . choices) ns

serializeNodes :: QNode -> String
serializeNodes node = ushow $ go (Set.singleton (Types.name node)) [node] []
  where
  go :: Set Text -> [QNode] -> [Node] -> [Node]
  go _seen [] acc = acc
  go seen (Types.QNode{..}:rest) acc =
    go newSeen (uniqueNeededNodes ++ rest) (Node{choices = choices',..}:acc)
    where
    newSeen = Set.union seen $ Set.fromList uniqueNeededNodesNames
    uniqueNeededNodesNames = map Types.name uniqueNeededNodes
    uniqueNeededNodes =
      Map.elems $ Map.fromList $ map (\a -> (Types.name a, a)) $
        filter (not . (`Set.member` seen) . Types.name) neededNodes
    (choices', neededNodes) =
      unzip $ map (\(Types.Choice msg (n@Types.QNode {Types.name = name'}))
                     -> (Choice msg name', n)) choices
