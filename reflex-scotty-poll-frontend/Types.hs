module Types where

import Data.Text (Text)

data QNode = QNode
  { name :: Text
  , intro :: Text
  , background :: Text
  , picture :: Text
  , choices :: [Choice]
  } deriving Show

data Choice = Choice 
  { msg :: Text
  , next :: QNode
  } deriving Show

data Answer = Answer
  { question :: Text
  , answer :: Text
  , nextNode :: QNode
  } deriving Show
