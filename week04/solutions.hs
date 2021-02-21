import Data.List

-- Exercise 1: Wholemeal programming
{-
Use wholemeal programming practices, breaking each function into a pipeline of incremental transformations to an entire data structure.
-}
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =
  sum . filter even . takeWhile (> 1)
    . iterate
      ( \n ->
          if even n
            then n `div` 2
            else 3 * n + 1
      )

-- Exercise 2: Folding with trees
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq, Ord)

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr insert Leaf
  where
    insert x Leaf = Node 0 Leaf x Leaf
    insert x (Node _ left root right)
      | left > right = Node (level newRight + 1) left root newRight
      | otherwise = Node (level newLeft + 1) newLeft root right
      where
        newRight = insert x right
        newLeft = insert x left

level :: Tree a -> Integer
level Leaf = 0
level (Node n _ _ _) = n

-- Exercise 3: More folds!
-- 1.
-- returns True if and only if there are an odd number of True values contained in the input list. It does not matter how many False values the input list contains
xor :: [Bool] -> Bool
xor = odd . sum . map (const 1) . filter id

xor' :: [Bool] -> Bool
xor' = odd . foldl countTrue 0 where countTrue acc x = if x then acc + 1 else acc

-- 2.
-- Implement map as a fold in such a way that map’ behaves identically to the standard map function
map' :: (a -> b) -> [a] -> [b]
map' f = foldr work [] where work x acc = f x : acc

-- 3.
-- Implement foldl using foldr in such a way that myFoldl behaves identically to the standard foldl function.
{-
The first parameters, i.e., the function, taken by foldl and foldr do a similar job, which is putting a new element into the accumulated list, but the the difference is the sequence of the functions parameters.
foldl's first parameter takes the list as its first argument; while foldr's first parameter takes the list as its second argument.
-}
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base xs

-- Exercise 4: Finding primes
-- Given an integer n, your function should generate all the odd prime numbers up to 2n + 2.
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+ 1) . (* 2)) $ [1 .. n] \\ sieve
  where
    sieve =
      map (\(i, j) -> i + j + 2 * i * j)
        . filter (\(i, j) -> i + j + 2 * i * j <= n)
        $ cartProd [1 .. n] [1 .. n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]