{-# LANGUAGE OverloadedStrings #-}
module Questions (initialQNode) where

-- Mock data
import Types
import Data.Text (Text)

description :: Text
description = "Example poll"

img :: Text
img = "bg.jpg"

initialQNode :: QNode
initialQNode = QNode
  { intro = description
  , name = "start"
  , background = img
  , picture = ""
  , choices =
    [ Choice
      { msg = "Start!"
      , next = node1
      }
    ]
  }

node1 :: QNode
node1 =
  QNode "node1" "Example question" img ""
  [ Choice "Answer A" nodeA
  , Choice "Answer B" nodeB
  ]

nodeA :: QNode
nodeA =
  QNode "nodeA" "You chose A" img ""
  [ Choice "Go to end" nodeEnd
  ]

nodeB :: QNode
nodeB =
  QNode "nodeB" "You chose B" img ""
  [ Choice "Go to end" nodeEnd
  ]

nodeEnd :: QNode
nodeEnd =
  QNode "nodeEnd" "The End." img ""
  [
  ]
