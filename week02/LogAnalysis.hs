{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Log file parsing

-- Exercise 1
-- The first step is figuring out how to parse an individual message
-- parses an individual line from the log file
parseMessage :: String -> LogMessage
parseMessage line =
  let wordList = words line
   in case wordList of
        ("I" : timestamp : msg) -> LogMessage Info (read timestamp) (unwords msg)
        ("W" : timestamp : msg) -> LogMessage Warning (read timestamp) (unwords msg)
        ("E" : severity : timestamp : msg) -> LogMessage (Error (read severity)) (read timestamp) (unwords msg)
        _ -> Unknown (unwords wordList)

-- Once we can parse one log message, we can parse a whole log file.
-- parses an entire log file at once and returns its contents as a list of LogMessages.
parse :: String -> [LogMessage]
parse logs = map parseMessage $ lines logs

-- Putting the logs in order
{-
Designed a data structure, a binary search tree of LogMessages, which is the Message Tree
A MessageTree should be sorted by timestamp:
1. The timestamp of a LogMessage in any Node should be greater than all timestamps of any LogMessage in the left subtree, and less than all timestamps of any LogMessage in teh right child
2. Unknown messages should not be stored in a MessageTree since they lack a timestamp
-}

-- Exercise 2
-- inserts a new LogMessage into an existing MessageTree, producing a new MessageTree.
-- insert may assume that it is given a sorted MessageTree, and must produce a new sorted MessageTree containing the new LogMessage in addition to the contents of the original MessageTree.
-- if insert is given a LogMessage which is Unknown, it should return the MessageTree unchanged.
insert :: LogMessage -> MessageTree -> MessageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage1@(LogMessage _ timestamp1 _) tree@(Node left logMessage2@(LogMessage _ timestamp2 _) right)
  | timestamp1 == timestamp2 = tree
  | timestamp1 < timestamp2 = Node (insert logMessage1 left) logMessage2 right
  | timestamp1 > timestamp1 = Node left logMessage2 (insert logMessage1 right)
insert _ tree = tree

-- Exercise 3
-- Once we can insert a single LogMessage into a MessageTree, we can build a complete MessageTree from a list of messages.
-- builds up a MessageTree containing the messages in the list, by successively inserting the messages into a MessageTree (beginning with a Leaf).
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
-- takes a sorted MessageTree and produces a list of all the LogMessages it contains, sorted by timestamp from smallest to biggest.
-- This is known as an in-order traversal of the MessageTree.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMessage right) = inOrder left ++ [logMessage] ++ inOrder right

-- Log file postmortem
-- Now that we can sort the log messages, the only thing left to do is extract the relevant information.
-- We have decided that “relevant” means “errors with a severity of at least 50”.

-- Exercise 5
-- takes an unsorted list of LogMessages, and returns a list of the messages corresponding to any errors with a severity of 50 or greater, sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = getMessage . inOrder . build . filter (isAboveSeverityLevel 50)

getMessage :: [LogMessage] -> [String]
getMessage (LogMessage _ _ msg : msgs) = msg : getMessage msgs
getMessage _ = []

isAboveSeverityLevel :: Int -> LogMessage -> Bool
isAboveSeverityLevel level (LogMessage (Error errorLevel) _ _)
  | level < errorLevel = True
  | otherwise = False
isAboveSeverityLevel _ _ = False

-- Epilogue

-- Exercise 6 (Optional)
-- For various reasons we are beginning to suspect that the recent mess was caused by a single, egotistical hacker. Can you figure out who did it?

exercise1 = do
  print $ parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
  print $ parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
  print $ parseMessage "This is not in the right format" == Unknown "This is not in the right format"
  print =<< testParse parse 5 "error.log"

main = do
  exercise1
  print =<< testWhatWentWrong parse whatWentWrong "error.log"