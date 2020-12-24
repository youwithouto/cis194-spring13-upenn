module Golf where

-- Exercise 1 Hopscotch
{-
The output of skips is a list of lists.
The first list in the output should be the same as the input list.
The second list in the output should contain every second element from the input list. . . and the nth list in the output should contain every nth element from the input list.
-}
skips :: [a] -> [[a]]
skips lst = [skipN i lst | i <- [1 .. length lst]]

skipN :: Int -> [a] -> [a]
skipN n lst = [lst !! i | i <- [n - 1, n - 1 + n .. length lst - 1]]

-- Exercise 2 Local maxima
{-
A local maximum of a list is an element of the list which is strictly
greater than both the elements immediately before and after it.
-}
localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : zs)
  | x < y && y > z = y : localMaxima (y : z : zs)
  | otherwise = localMaxima (y : z : zs)
localMaxima _ = []

-- Exercise 3 Histogram
{-
takes as input a list of Integers between 0 and 9 (inclusive), and outputs a vertical histogram showing how many of each number were in the input list.

- Need to build the graph bottom-up
- Need a function that have a parameter `level` indicating the current level of the stars
- Need a map from number (0 - 9) to their corresponding number of stars
-}
histogram :: [Integer] -> String
histogram xs = unlines (map (starLine counts) [maxLevel, maxLevel - 1 .. 1]) ++ "==========\n0123456789\n"
  where
    counts = numCount xs
    maxLevel = maximum counts

-- starLine returns a single line of stars
starLine :: [Int] -> Int -> String
starLine xs n = [if i >= n then '*' else ' ' | i <- xs]

-- numCount counts the number of 0 ~ 9 in the user input
numCount :: [Integer] -> [Int]
numCount xs = map (\n -> length $ filter (== n) xs) [0 .. 9]
