{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parse :: String -> [LogMessage]
parse logs = map (parseMessage) (lines logs)

parseMessage :: String -> LogMessage
parseMessage msg = case tokens of
    ("I":_) -> LogMessage Info n1 (restAfter 2 tokens)
    ("W":_) -> LogMessage Warning n1 (restAfter 2 tokens)
    ("E":_) -> LogMessage (Error n1) n2 (restAfter 3 tokens)
    _   -> Unknown msg
    where tokens = words msg
          n1 = read (tokens !! 1) :: Int
          n2 = read (tokens !! 2) :: Int
          restAfter n wordList = unwords $ drop n wordList

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg (Leaf) = Node Leaf msg Leaf
insert msg@(LogMessage _ time _) (Node left p@(LogMessage _ parentTime _) right)
  = if (time <= parentTime)
      then (Node (insert msg left) p right)
      else (Node left p (insert msg right))
insert _ (Node _ (Unknown _) _) = error "Invalid tree: has Unknown MessageType"

testMsgTransform :: LogMessage -> LogMessage
testMsgTransform (LogMessage Info time str) = LogMessage Warning time str
testMsgTransform (LogMessage Warning time str) = LogMessage Info time str
testMsgTransform msg = msg

tokenIndex :: Int -> [[Char]] -> Int
tokenIndex _ []     = 0
tokenIndex 0 (y:_)  = 1 + length y
tokenIndex _ (y:[]) = 1 + length y
tokenIndex x (y:zs) = 1 + length y + tokenIndex (x-1) zs

