# Homework 4

## Exercise 1: Wholemeal programming
Target:
- Reimplement each of the following functions in a more idiomatic Haskell style.
  - Use wholemeal programming practices, breaking each function into a pipeline of incremental transformations to an entire data structure. 

Definitions:
```haskell
fun1 :: [Integer] -> Integer
fun1 []  = 1
fun1 (x : xs)
    | even x    = (x – 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)
```

Solutions:
```haskell
fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter even
fun1 = product . map (subtract 2) . filter even 
```
- Similar logic as the example on the lecture note
- Need to find the functions, `product` and `subtract`

```haskell
fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)
```
- `fun2` is repeated applied to an integer, hence, `iterate f x == [x, f x, f (f x), ...]`


## Exercise 2: Folding with trees
Background:
- The definition of a binary tree data structure.
- The height of a binary tree is the length of a path from the root to the deepest node.
- A binary tree is balanced if the height of its left and right subtrees differ by no more than 1, and its left and right subtrees are also balanced.

Definitions:
```haskell
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)
```

```haskell
-- generate a balanced binary tree from a list of values using `foldr`
foldTree :: [a] -> Tree a
```

Solution:
```haskell
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Insert the new node into the lower subtree
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h left root right)
    | h1 < h2 = Node h (insert x left) root right
    | h1 > h2 = Node h left root (insert x right)
    | otherwise = Node (h3 + 1) left' root right
    where h1 = height left
          h2 = height right
          h3 = height left'
          left' = insert x left

-- Need to distinguish between a node with height 0 and a leaf
height :: Tree a -> Integer
height Left           = -1
height (Node h _ _ _) = h
```
- Insert a new node into a balanced tree and returns a new balanced tree containing that node
- Just need to consider the height, it doesn't require the tree to be a search tree (ordered)


## Exercise 3: More folds!

### 1

Target:
- Implement a function which returns `True` if and only if there are an odd number of `True` values contained in teh input list
- Does not matter how many `False` values the input list contains

Definition:
```haskell
xor :: [Bool] -> Bool
```

Examples:
```haskel
xor [False, True, False] == True
xor [False, True, False, False, True] == False
```

Solution:
```haskell
xor :: [Bool] -> Bool
xor = foldr (/=) False
```

### 2

Target:
- Implement `map` as a `fold`

Definition:
```haskell
map' :: (a -> b) -> [a] -> [b]
```

Solution:
```haskell
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\e l -> f e : l) []
map' f = foldr ((:) . f) []
```

### 3
Target:
- Implement `foldl` using `foldr`

Definition:
```haskell
myFoldl :: (a -> b -> a) -> a -> [b] -> a
```

Solution:
```haskell
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f acc xs = foldr (flip f) acc xs
myfoldl f = foldr (flip f)
myfoldl = foldr . flip
```
- The first parameters taken by `foldl` and `foldr` are different. 
  - The parameter is a function which performs operation with the current value and the accumulated value and depending on which function (`foldl` or `foldr`) it's being used with, its paramter sequence is different


## Exercise 4: Finding primes
Target:
- Implement the algorithm using function composition
- Given an integer n, your function should generate all the odd prime numbers up to `2n + 2`

Definition:
```haskell
sieveSundaram :: Integer -> [Integer]
```

Solution:
```haskell
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 
                (\x -> 2 * x + 1) <$> [1..n] \\ crossOut
                where crossOut = [x | i <- [1..n], j <- [i..n], i + j + 2 * i * j <= n]

```