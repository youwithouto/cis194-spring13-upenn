# Log Analysis



## Log file parsing

Background:

- Log file error.log with a different log message on each line
  - Each line begins with a character indicating the type of log message it represents
  - The error message lines have an integer indicating the severity of the error
    - 1 (low) ~ 100 (high)
  - All the types of messages have an integer timestamp followed by textual content that runs to the end of the line
- Types of log messages
  - "I" for informations messages
  - "W" for warnings
  - "E" for errors

Definitions:

```haskell
-- a new datatype for message type
data MessageType = Info
								 | Warning
								 | Error Int
								 deriving (Show, Eq)
								 
-- a type synonym for timestamp								 
type TimeStamp = Int

-- a new datatype for the entire log message
data LogMessage = LogMessage MessageType TimeStamp String
								| Unknown String
								deriving (Show, Eq)
```

Related Haskell concepts:

- Modules



### Exercise 1

Target:

- Parse an individual message
- Parse a whole log file

Definition:

```haskell
-- parse a single log row
parseMessage : String -> LogMessage

-- parse a whole log file
parse :: String -> [LogMessage]
```

Examples:

```haskell
parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"

parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"

parseMessage "This is not in the right format" == Unknown "This is not in the right format"
```

Solution:

```haskell
parseMessage : String -> LogMessage
parseMessage line = 
	let wordList = words line
		in case wordList of
			("I" : timestamp : msg) 					 -> LogMessage Info (read timestamp) (unwords msg)
			("W" : timestamp : msg)  					 -> LogMessage Warning (read timestamp) (unwords msg)
			("E" : severity : timestamp : msg) -> LogMessage (Error (read severity)) (read timestamp) (unwords msg)
			_ -> Unknown (unwords wordList)

parse :: String -> [LogMessage]
parse logs = map parseMessage $ lines logs
parse = map parseMessage $ lines -- point-free
parse = parseMessage <$> lines -- <$> is the infix version of fmap

```

Related Haskell Concepts:

- `words :: String -> [String]`
- `unwinds :: [String] -> String`



## Putting the logs in order

Target:

- Sort the logs
  - Sort by timestamp
  - Unknown messages should not be stored in a MessageTree 
- Use a binary search tree to do the sorting

Definition:

```Haskell
data MessageTree = Leaf
								 | Node MessageTree LogMessage MessageTree 
```



### Exercise 2

Definition:

```Haskell
insert :: LogMessage -> MessageTree -> MessageTree
```

Solution:

```Haskell
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert log Leaf = Node Leaf log Leaf
insert log1@(_ ts1 _) (Node left log2@(_ ts2 _) right)
	| ts1 < ts2 = Node (insert log1 left) log2 right
	| otherwise = Node left log2 (insert log1 right)
insert _ (Node _ (Unknown _) _) = error "Internal error"  -- always work with total functions
```

Related Haskell Concepts:

- `@()` pattern matching



### Exercise 3

Target:

- Build a complete MessageTree from a list of messages

Definition:

```haskell
build :: [LogMessage] -> MessageTree
```

Solution:

```haskell
build :: [LogMessage] -> MessageTree
build log = foldr insert Leaf log
build = foldr insert Leaf -- point-free
```

Related Haskell Concepts:

- `foldr`



### Exercise 4

Definition:

```Haskell
inOrder :: MessageTree -> [LogMessage]
```

Solution:

```haskell
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder Node (left log right) = (inOrder left) ++ [log] ++ (inOrder right)
```

Related Haskell Concepts:

- List concatenation



## Log file postmotem



### Exercise 5

Target:

- Extract relevant information
  - Error eiwth a severity of at least 50

Definition:

```Haskell
whatWentWrong :: [LogMessage] -> [String]
```

Solution:

```haskell
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong logs = map getContent $ filter isRelevant $ inOrder . build $ logs
whatWentWrong = map getContent $ filter isRelevant $ inOrder . build  -- point-free
whatWentWrong = getContent <$> filter isRelevant $ inOrder . build  -- <$>

isRelevant :: LogMessage -> Bool
isRelevant LogMessage (Error (read severity)) _ _ = severity >= 50
isRelevant _ = False

getContent :: LogMessage -> String
getContent LogMessage (Error _) _ msg = msg
getContent _ = ""
```

Related Haskell Concepts:

- `map`
- `fmap` and `<$>`
- `filter`



## Miscellaneous



### Exercise 6 (Ignore)