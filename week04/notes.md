# Week 04 | Higher-Order Programming and Type Inference

## Anonymous Functions

```haskell
greaterThan100_2 :: [Integer] -> [Integer]
greaterThan100_2 xs = filter (\x -> x > 100) xs
greaterThan100_3 xs = filter (>100) xs
```
- The backslash is supposed to look kind of like a lambda with the short leg missing
- `(>100)` is an **operator section**: 
  - if `?` is an operator, then `(?y)` is equivalent to the function `\x -> x ? y`, and `(y?)` is equivalent to `\x -> y ? x`. 
  - Using an operator section allows us to partially apply an operator to one of its two arguments.
    - What we get is a function of a single argument


## Function Composition

`.` is called **function composition**

`f` and `g` are functions, then `f . g` is the function which does first `g` and then `f`

Function composition can be quite useful in writing concise, elegant code. 
- it fits well in a "wholemeal" style where we think about composing together successive high-level transformation of a data structure

```haskell
myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))
myTest' = even . length . greaterThan100
```

## Currying and Partial Application

> All functions in Haskell take only one argument. 

The idea of representing multi-argument functions as one-argument functions returning functions is known as **currying**.


### Partial Application

The idea of **partial application** is that we can take a funciton of multiple arguments and apply it to just some of its arguments, and get out a function of the remaining arguments. 

Haskell doesn't make it easy to partially apply to an argument other than the first.
The one exception is infix operators, which can be partially applied to either of their two arguments using an operator section. 

There is an art to deciding the order of arguments to a function to make partial applications of it as useful as possible:
- The arguments should be ordered from least to greatest variation, that is, arguments which will often be the same should be listed first, and arguments which will often be different should come last. 

### Wholemeal Programming

```haskell
foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7*x + 2) + foobar xs
  | otherwise = foobar xs
```
- Not good haskell style
  - doing too much at once
  - working at too low of a level

Making incremental transformation to the entire input.

```haskell
foobar' = sum . map (\x -> 7 * x + 2) . filter (>3)
```

This style of coding in which we define a function without reference to its arguments, in some sense saying what a function is rather than what it does, is known as **point-free** style

## Folds

Another recursion pattern

Combine the elements of the list into a final answer

`foldr` and `foldl`
- You should use `foldl'` from `Data.List`