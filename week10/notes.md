# Week 10 | Applicative Functors | I

## Motivation

```haskell
type Name = String

data Employee = Employee { name    :: Name
                         , phone   :: String }
                deriving Show
```
- If we have a `Name` and a `String`, we can apply the `Employee` constructor to build an Employee object
- What if we don't have a `Name` and a `String`; what we actually have is a `Maybe Name` and a `Maybe String`
  - Perhaps they came from some file full of errors, or from a form where some of the fields might have been left blank, or something of that sort. 
  - We cannot make an `Employee` but we can make a `Maybe Employee`

```haskell
-- Can we do this?
(Name -> String -> Employee) -> (Maybe Name -> Maybe String -> Maybe Employee)
```
- Yes

```haskell
-- Can we do this?
(Name -> String -> Employee) -> ([Name] -> [String] -> [Employee])

-- or, suppose e is some large data structure
(Name -> String -> Employee) -> ((e -> Name) -> (e -> String) -> (e -> Employee))
```
- Yes




## Generalising

```haskell
-- We are trying to do this
(a -> b -> c) -> (f a -> f b -> f c)

-- Similar
fmap :: (a -> b) -> (f a -> f b)
```

...

## Applicative

Functors for which this sort of "contextual application" is possible are called `applicative`
The `Applicative` class, defined in `Control.Applicative` captures this pattern.

```haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```
- The `(<*>)` operator (often pronounced "ap", short for "apply") encapsulates exactly this principle of "contextual application"
- The `Applicative` class requires its instances to be instances of `Functor` as well, we can always use `fmap` with `Applicative`
- The `pure` method lets us inject a value of type `a` into a container
  - `fmap0` would be another reasonable name for `pure`


```haskell
pure  :: a             -> f a
fmap  :: (a -> b)      -> f a -> f b
fmap2 :: (a -> b -> c) -> f a -> f b -> f c
```
- With `(<*>)` we can implement `fmap2`, which in the standard library is actually called `liftA2`

```haskell
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 h fa fb = (h `fmap` fa) <*> fb
```

`Control.Applicative` defines `(<$>)` as a synonym for `fmap`

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
```

Therefore

```haskell
liftA2 h fa fb = h <$> fa <*> fb
```

and 

```haskell
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 h fa fb fc = ((h <$> fa) <*> fb) <*> fc
```
- Unlike the jump from `fmap` to `liftA2`, which requires generalising from `Functor` to `Applicative`, going from `liftA2` to `liftA3` (and from there to `liftA4`, ...) requires no extra power, `Applicative` is enough

- When we have all the arguments like this we usually don’t bother calling `liftA2`, `liftA3`, and so on, but just use the `f <$> x <*> y <*> z <*>` ... pattern directly. 
  - `liftA2` and friends do come in handly for partial application, however.

But what about `pure`? 
- `pure` is for situations where we want to apply some function to arguments in the context of some functor f, but one or more of the arguments is not in f — those arguments are “pure”, so to speak. - We can use `pure` to lift them up into f first before applying. 

```haskell
liftX :: Applicative f => (a -> b -> c -> d) -> f a -> b -> f c -> f d
liftX h fa b fc = h <$> fa <*> pure b <*> fc
```

## Applicative Laws

```haskell
f `fmap` x === pure f <*> x
```

- Mapping a function f over a container x ought to give the same results as first injecting the function into the container, and then applying it to x with `(<*>)`.
- There are other laws, but they are not as instructive


## Applicative Examples

- `pure` works by injecting a value into a `Just` wrapper; 
- `(<*>)` is function application with possible failure.
  - The result is Nothing if either the function or its argument are.

```haskell
instance Applicative Maybe where
  pure              = Just
  Nothing <*> _     = Nothing
  _ <*> Nothing     = Nothing
  Just f <*> Just x = Just (f x)

Let’s see an example:

m_name1, m_name2 :: Maybe Name
m_name1 = Nothing
m_name2 = Just "Brent"

m_phone1, m_phone2 :: Maybe String
m_phone1 = Nothing
m_phone2 = Just "555-1234"

ex01 = Employee <$> m_name1 <*> m_phone1
ex02 = Employee <$> m_name1 <*> m_phone2
ex03 = Employee <$> m_name2 <*> m_phone1
ex04 = Employee <$> m_name2 <*> m_phone2
```