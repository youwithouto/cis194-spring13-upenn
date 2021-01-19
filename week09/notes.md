# Week 09 | Functors

## Motivation

Over the past weeks we have seen a number of functions designed to "map" a function over every element of some sort of container. 
- `map`
- `treeMap`
There's a repeated pattern here, and as good Haskell programmers we want to know how to generalise it.
- Which parts are the same from example to example?
- Which parts are different?
  - The container being "mapped over"

```haskell
thingMap :: (a -> b) –> f a -> f b
```
- What sort of things are these "containers"?
- Can we assign a type variable like `f` to them? -> Can we use a type to represent their pattern?


## A brief digression on kinds

Just as every expression has a type, types themselves have "types", called `kinds`. -> ghci `:kind` | `:k`

Every type which can actually serve as the type of some values has kind `*`

```
:k Int       -> Int       :: *
:k Bool      -> Bool      :: *
:k Char      -> Char      :: *
:k Maybe Int -> Maybe Int :: *
```

There are values of type `Maybe Int`, and of type `Maybe Bool`, but not of type `Maybe`
- But `Maybe` is a valid **type-like-thing**

```
:k Maybe -> Maybe :: * -> *
```
- Maybe has kind `* -> *`

> `Maybe` is, in a sense, a function on types.
- Usually, we call it a `type constructor`
  - `Maybe` takes as input types of kind `*`, and produces another type of kind `*`


```haskell
-- JoinList takes two types as parameters and give us back a new type
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)

:k JoinList -> JoinList :: * -> * -> *                  
```

```haskell
data Funny f a = Funny a (f a)

:k Funny -> Funny :: (* -> *) -> * -> *
```
- Funny is a `higher-order type constructor`, in the same way that `map` is a `higher-order function`


## Functor

The essence of the mapping pattern we saw was a higher-order function with a type like `thingMap :: (a -> b) -> f a -> f b` where `f` is a type variable standing in for some type of *kind* `* -> *`

Can we write a function of this type once and for all?

```haskell
thingMap :: (a –> b) -> f a –> f b
thingMap h fa = ???
```
- No. 
  - There's not much we can do if we don't know what `f` is. 
    - `thingMap` has to work differently for each particular `f`

- The solution is to make a `typeclass`, which is traditionally called `Functor`. 

```haskell
class Fucntor f where
    fmap :: (a -> b) -> f a -> f b
```
- defined in the standard Prelude. 
- The name "functor" comes from **category theory**, and not the same thing as functors in C++ (first-class functions). 

- Now we can implement this class in a way specific to each particular `f`. 
  - The `Functor` class abstracts over types of kind `* -> *`


```haskell
instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap h (Just a) = Just (h a)


instance Functor [] where
    fmap _ []       = []
    fmap f (x : xs) = f x : fmap f xs
```