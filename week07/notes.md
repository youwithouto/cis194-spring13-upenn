# Week 07 | Folds and Monoids

## Folds, again
We can generalise the idea of folding to other data types. 

```haskell
data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty
```

Patterns:
1. takes a Tree as input
2. pattern-matches on the input Tree
3. in the Empty case, gives a simple answer
4. in the Node case:
   1. calls itself recursively on both subtrees
   2. somehow combines the results from the recursive calls with the data x to produce the final result

Abstract out repeating patterns:
- Pass as parameters the parts of the above examples which change from case to case
    1. The return type
    2. The answer in the Empty case
    3. How to combine the recursive calls

```haskell
treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty        = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)
```

### Folding Expressions



### Folds in General
We can implement a fold for many (though not all) data types. 
– The fold for T will take one (higher-order) argument for each of T's constructors, encoding how to turn the values stored by that constructor into a value of the result type, assuming that any recursive occurrrences of T have already been folded into a result. 
  - Many functions we might want to write on T will end up being expressible as simple folds. 



## Monoids
Another standard type class found in the `Data.Monoid` module

```haskell
class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m

    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

(<>) :: Monoid m => m -> m -> m
(<>) = mappend
```
- `(<>)` is defined as a synonym for `mappend`, simply because writing "mappend" is tedious.

Types which are instances of `Monoid` have a special element called `mempty`, and a binary operation `mappend` (abbreviated `(<>)`) which takes two values of the type and produces another one. 
- The intention is that `mempty` is an identity for `<>`, and `<>` is associative
  - that is, for all x, y, and z
    - `mempty <> x == x`
    - `x <> mempty == x`
    - `(x <> y) <> z == x <> (y <> z)`
  - The associativity law means that we can unambiguously write things like `a <> b <> c <> d <> e`
    - because we will get the same result no matter how we parenthesize.

There is also `mconcat`, for combining a whole list of values. 
- By default it is implemented using `foldr`, but it is included in the `Monoid` class since particular instances of `Monoid` may have more efficient ways of implementing it. 


`Monoids` shows up everywhere

– Lists form a monoid under concatenation

```haskell
instance Monoid [a] where
mempty  = []
mappend = (++)
```

- Addition defines a perfectly good monoid on integers (or rational numbers, or real number, ...)
  - So does multiplication
- We cannot give two different instancres of the same type class to the same type. 
  - Instead, we create two `newtypes`, one for each instance

```haskell
newtype Sum a = Sum a
  deriving (Eq, Ord, Num, Show)

getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Monoid (Sum a) where
  mempty  = Sum 0
  mappend = (+)

newtype Product a = Product a
  deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a => Monoid (Product a) where
  mempty  = Product 1
  mappend = (*)
```
- to find the product of a list of `Integers` using `mconcat`, we have to first turn them into values of type `Product Integer`


- Pairs form a monoid as long as the individual components do
