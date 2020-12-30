# Week 07 | Folds and Monoids

## Folds

- **Folds** can be used to implement any function where you traverse a list once, element by element, and then return something based on that. 
- **Folds** reduce the list to some single value.  

- A fold takes *a binary function*, *a starting value* (I like to call it the `accumulator`) and *a list* to fold up.
  - The binary function itself takes two parameters. 
  - The binary function is called with the accumulator and the first (or last) element and produces a new accumulator. 
  - Then, the binary function is called again with the new accumulator and the now new first (or last) element, and so on. 
  - Once we've walked over the whole list, only the accumulator remains, which is what we've reduced the list to.
- The type of the accumulator value and the end result is always the same when dealing with folds.

### The `foldl` function (the left fold)

It folds the list up from the left side.

The binary function is applied between the starting value and the head of the list. 
That produces a new accumulator value and the binary function is called with that value and the next element, etc.

### The `foldr` function (the right fold)

The accumulator eats up the values from the right, and it has the current value as the first parameter and the accumulator as the second one. 

`foldr` has the accumulator on the right

The right fold works on infinite lists, whereas left ones don't. 

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

Type classes in Haskell are used to present an interface for types that have some behaviour in common. 

Examples:
- `Eq`: for types whose values can be equated
- `Ord`: for things that can be put in an order
- `Functor`
- `Applicative`

When we make a type, we think about which behaviour it supports, i.e., what it can act like and then based on that we decide which type classes to make it an instance of. 

---

A monoid is when you have an associative binary function and a value which acts as an identity with respect to that function. 
- When something acts as an identity with respect to a function, it means that when called with that function and some other value, the result is always equal to that other value. 

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
- Only concrete types can be made instances of `Monoid`, because the "m" in the type class definition doesn't take any type parameters. 
  - This is different from `Functor` and `Applicative`, which require their instances to be type constructors which take one parameter. 
- `mempty` is not really a function, since it doesn't take parameters, so it's a polymorphic constant, representing the identity value for a particular monoid. 
- `mappend` is the binary function, which takes two values of the same type and returns a value of that type as well.
  - The decision to name `mappend` as it's named was kind of unfortunate, because it implies that we're appending two things in some way. 
    - Most `Monoid` instances does not "append" values and `mappend` is just a binary function that takes two monoid values and returns a third. 
  - `(<>)` is defined as a synonym for `mappend`, simply because writing "mappend" is tedious.
- `mconcat` takes a list of monoid values and reduces them to a single value by doing `mappend` between the list's elements. 
  - It has a default implementation which just takes `mempty` as a starting value and folds the list from the right with `mappend`
- When making a type an instance of `Monoid`, it suffices to just implement `mempty` and `mappend`.
  - The default implementation for `mconcat` is fine for most instances
  - The reason `mconcat` is there at all is because for some instances, there might be a more efficient way to implement `mconcat`, but for most instances the default implementation is just fine. 

### Lists are `monoids`

Lists are monoids
- the `++` function and the empty list `[]` form a monoid

```haskell
instance Monoid [a] where
  mempty = []
  mappend = (++)
```

### `Product` and `Sum`

- The binary function `*` and the identity value `1`
- The binary function `+` and the identity value `0`

There are two equally valid ways for numbers to be monoids, which way to choose?
- When there are several ways for some type to be an instance of the same type class, we can wrap that type in a `newtype` and then make the new type an instance of the type class in a different way. 

```haskell
newtype Sum a = Sum a
  deriving (Eq, Ord, Num, Show)

getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Monoid (Sum a) where
  mempty  = Sum 0
  mappend = (+)

```

```haskell

newtype Product a = Product a
  deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a => Monoid (Product a) where
  mempty  = Product 1
  mappend = (*)
```

```haskell
newtype Product a =  Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded)  
```

### `Any` and `All`

Another type which can act like a monoid in two distinct but equally valid ways is `Bool`.
- The or function `||` acts as the binary function and `False` as the identity value
- The and function `&&` acts as the binary funciton and `True` as the identity value


```haskell
newtype Any = Any { getAny :: Bool }  
    deriving (Eq, Ord, Read, Show, Bounded)  

instance Monoid Any where  
        mempty = Any False  
        Any x `mappend` Any y = Any (x || y)  
```

```haskell
newtype All = All { getAll :: Bool }  
        deriving (Eq, Ord, Read, Show, Bounded)  

instance Monoid All where  
        mempty = All True  
        All x `mappend` All y = All (x && y)  
```

With lists, numbers, and boolean values, finding monoids is just matter of looking at already existing commonly used functions and seeing if they exhibit some sort of monoid behaviour. With `Ordering`, we have to look a bit harder to recognise a monoid. 

### The `Ordering` monoid

The `Ordering` type is used when comparing things and it can have three values: `LT`, `EQ` and `GT`.

```haskell
instance Monoid Ordering where  
    mempty = EQ  
    LT `mappend` _ = LT  
    EQ `mappend` y = y  
    GT `mappend` _ = GT  
```
- wehn we use `mappend`, its left parameter is always kept unless it's `EQ`

The instance is set up like this:
- when we mappend two `Ordering` values, the one on the left is kept, unless the value on the left is `EQ`, in which case the right one is the result. 
- The identity is `EQ`. 
- At first, this may seem kind of arbitrary, but it actually resembles the way we alphabetically compare words. 
  - We compare the first two letters and if they differ, we can already decide which word would go first in a dictionary. 
  - However, if the first two letters are equal, then we move on to comparing the next pair of letters and repeat the process.

> The `Ordering` monoid allows us to easily compare things by many different criteria and put those criteria in an order themselves, ranging from the most important to the least. 

### `Maybe` the monoid

The various ways that `Maybe a` can be made an instance of `Monoid`

- Treat `Maybe a` as a monoid only if its type parameter `a` is a monoid as well and then implement `mappend` in such a way that it uses the `mappend` operation of the values that are wrapped with `Just`.
  - We use `Nothing` as the identity, and so if one of the two values that we're `mappend`ing is `Nothing`, we keep the other value. 

```haskell
instance Monoid a => Monoid (Maybe a) where  
    mempty = Nothing  
    Nothing `mappend` m = m  
    m `mappend` Nothing = m  
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)  
```
- Notice the class constraint.
  - It says that `Maybe a` is an instance of `Monoid` only if `a` is an instance of `Monoid`.
- If we `mappend` something with a `Nothing`, the result is that something. 
- If we `mappend` two `Just` values, the contents of teh `Just`s get `mappend`ed and then wrapped back in a `Just`.
  - We can do this becuase the class constraint ensures that the type of what's inside the `Just` is an instance of `Monoid`.

> This comes in use when you're dealing with monoids as results of computations that may have failed.
> Because of this instance, we don't have to check if the computation have failed by seeing if they're a `Nothing` or `Just`; we can just continue to treat them as normal monoids.

What if the type of the content of the `Maybe` aren't an instance of `Monoid`?



### Using monoids to fold data structures

> One of the more interesting ways to put monoids to work is to make them help us define folds over various data structures. 

The `Foldable` type class was introduced because there are so many data structures that work nicely with folds. 
- `Fordable` is for things that can be folded up.
- `Data.Foldable`
  - It exports functions whose names clash with the one sfrom teh `Prelude`
  - `import qualified Foldable as F`

```haskell
instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r  
```


----


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


- to find the product of a list of `Integers` using `mconcat`, we have to first turn them into values of type `Product Integer`


- Pairs form a monoid as long as the individual components do
