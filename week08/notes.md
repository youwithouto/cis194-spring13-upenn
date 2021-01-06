# Week 08 | IO

## The Problem with Purity
Haskell is *lazy* and *pure*:
- Functions may not have any external effects. 
- Functions may not depend on external stuff and they may only depend on their inputs.
  - Functions should give the same output for the same input every time. 

If the only thing we could do with Haskell is writing functions which we can then evaluate at the ghci prompt, it would be theoretically interesting but practically useless. 

## The IO Type
The solution to the problem is a special type called `IO`. 
- Values of type `IO a` are descriptions of effectful computations, which, if executed would (possibly) perform some effectful I/O operations and (eventually) produce a value of type `a`.
  - A value of type `IO a` in and of iteself, is just an insert, perfectly safe thing with no effect. 
    - It is just a description of an effectful computation. 
    - A first-class imperative program. 
    - A value of type `IO a` is not executed.


How do values of `IO a` actually get executed?
- There is only one way:
  - The Haskell compiler looks for a special value `main :: IO ()`, which will actually get handed to the runtime system and executed. 
  - `main` can be arbitrarily complicated, and will usually be composed of many smaller `IO` computations.

```haskell
putStrLn :: String -> IO ()
main = putStrLn "Hello, Haskell!"
```


## There is no String "inside" an IO String
How do I get the `String` out of an `IO String`?
- A value of type `IO String` is a description of some computation, a way for generating a `String`.
  - There is no `String` inside an `IO String`.


## Combining IO

```haskell
(>>) :: IO a -> IO b -> IO b

(>>=) :: IO a -> (a -> IO b) -> IO b
```


## Record Syntax

```haskell
data D = C T1 T2 T3

-- vs. 

data D = C { field1 :: T1, field2 :: T2, field3 :: T3 }
```
- We specify not just a type but also a name for each field stored inside the C constructor. 
  - Each field name is automatically a projection function which gets the value of that field out of a value of type D. 
  - The is special syntax for constructinfg, modifying, and pattern-matching on values of type D (in addition to the usual syntax for such things). 