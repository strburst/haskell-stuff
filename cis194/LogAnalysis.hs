{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parse :: String -> [LogMessage]
parse logs = map (parseMessage) (lines logs)

parseMessage :: String -> LogMessage
parseMessage msg
    | tokens !! 0 == "I" = LogMessage Info token1 (unwords (drop 2 tokens))
    | tokens !! 0 == "W" = LogMessage Warning token1 (unwords (drop 2 tokens))
    | tokens !! 0 == "E" = LogMessage (Error token1) token2 (unwords (drop 3 tokens))
    | otherwise          = Unknown msg
    where tokens = words msg
          token1 = read (tokens !! 1) :: Int
          token2 = read (tokens !! 2) :: Int
          -- infoEndIW   = length (tokens !! 1) + 3
          -- infoEndE    = (length (tokens !! 1)) + (length (tokens !! 2)) + 4

-- insert :: LogMessage -> MessageTree -> MessageTree
-- insert msg (Leaf) = Node Leaf msg Leaf
-- insert (LogMessage msgType time str) (Node Leaf (LogMessage parentMsgType parentTime parentStr) Leaf) =

testMsgTransform :: LogMessage -> LogMessage
testMsgTransform (LogMessage Info time str) = LogMessage Warning time str
testMsgTransform (LogMessage Warning time str) = LogMessage Info time str
testMsgTransform msg = msg

tokenIndex :: Int -> [[Char]] -> Int
tokenIndex _ []     = 0
tokenIndex 0 (y:_)  = 1 + length y
tokenIndex _ (y:[]) = 1 + length y
tokenIndex x (y:zs) = 1 + length y + tokenIndex (x-1) zs

