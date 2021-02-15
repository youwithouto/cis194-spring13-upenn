# Golf





### Exercise 1 Hopscotch

Definition:

```Haskell
skips :: [a] -> [[a]]
```

Examples:

```haskell
skips "ABCD"			 == ["ABCD", "BD", "C", "D"]
skips "hello!"		 == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1]					 == [[1]]
skips [True,False] == [[True,False], [False]]
skips []           == []
```

Solution:

`skips` returns a list of lists

- Each element list in the return list is a sublist from the original list with certain elements removed
- A helper function, `skipEvery`, is need to implement `skips`
  - It takes the original list and an Integer indicating the number of elements to skip as argument
  - It returns a list with some of the elements skipped

```Haskell
skips :: [a] -> [[a]]
skips l = [skipEvery i l | i <- [0..n]] where n = length l - 1

skipEvery :: n [a] -> [a]
skipEvery n l = helper n (drop n l)
skipEvery n l = helper n . drop n l
skipEvery n = helper n . drop n  -- point-free

helper :: n [a] -> [a]
helper _ [] = []
helper n x:xs = x : helper (drop n xs)

```



### Exercise 2 Local maxima

Background:

- A local maximum of a list is an element of the list which is strictly greater than both the elements immediately before and after it

Definition:

```Haskell
localMaxima :: [Integer] -> [Integer]
```

Examples:

```Haskell
localMaxima [2,9,5,6,1] == [9,6]
localMaxima [2,3,4,1,5] == [4]
localMaxima [1,2,3,4,5] == []
```

Solution:

```Haskell
localMaxima :: [Integer] -> [Integer]
localMaxima l@(a:b:c:_)
	| b > a && b > c = b : localMaxima . tail l
	| otherwise 		 = localMaxima . tail l 
localMaxima _ 		 = []
```



### Exercise 3 Histogram

Definition:

```Haskell
histogram :: [Integer] -> String
```

Solution:

```Haskell
histogram :: [Integer] -> String
histogram ns = unlines (map (starRow counts) [maxLevel, maxLevel - 1 .. 1]) ++ "==========\n0123456789\n"
	where
		counts   = count ns
		maxLevel = maximum counts

starRow :: [Integer] -> Integer -> String
starRow ns n = [if i >= n then '*' else ' ' | i <- ns]

count :: Integer -> [Integer] -> Integer
count n l = length (filter (== n) l)
count n l = length . filter (== n) l
count n  = length . filter (== n)  -- point-free
```

