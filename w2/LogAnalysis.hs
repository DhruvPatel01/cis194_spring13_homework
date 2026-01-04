{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Data.Char (isDigit)
import Log

stripString :: String -> String
stripString (' ' : t) = stripString t
stripString a = a

isNumber :: String -> Bool
isNumber [] = False
isNumber (c : []) = isDigit c
isNumber (c : t) = isDigit c && isNumber t

getNumber :: String -> Int
getNumber s = getNumber' 0 s

getNumber' :: Int -> String -> Int
getNumber' acc [] = acc
getNumber' acc (h : t) = getNumber' (acc * 10 + (fromEnum h - fromEnum '0')) t

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  "I" : ts : msg | isNumber ts -> LogMessage Info (getNumber ts) (unwords msg)
  "W" : ts : msg | isNumber ts -> LogMessage Warning (getNumber ts) (unwords msg)
  "E" : lvl : ts : msg | isNumber lvl && isNumber ts -> LogMessage (Error (getNumber lvl)) (getNumber ts) (unwords msg)
  line -> Unknown (unwords line)

parse :: String -> [LogMessage]
parse c = map parseMessage (lines c)

---

insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts1 _) (Node left node@(LogMessage _ ts2 _) right)
  | ts1 < ts2 = Node (insert msg left) node right
  | otherwise = Node left node (insert msg right)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left node right) = inOrder left ++ (node : inOrder right)

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error lvl) _ _) = lvl >= 50
isRelevant _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ m) = m
getMessage (Unknown m) = m

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . inOrder . build . filter isRelevant