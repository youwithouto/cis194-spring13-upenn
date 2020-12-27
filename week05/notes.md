# Week 05 | More Polymorphism and Type Classes

Haskell's brand of polymorphism is known as parametric polymorphism
- This means that polymorphic functions must work uniformly for any input type. 

## Parametricity

`parametric` is a fancy term for "works uniforly for any type chosen by the caller"

`type variable` can stand for any type

The caller of a polymorphic function gets to choose the type. 

haskell types are erased by the compiler after being checked: 
- At runtime, there is no type information around to query



## Two View on Parametricity

As a user of polymorphic functions, parametricity corresponds not to restrictions but to guarantees. 
- In general, it is much easier to use and reason about tools when those tools give you strong guarantees as to how they will behave. 
- Parametricity is part of teh reason that just looking at the type of Haskell function can tell you so much about the function. 

We can use Haskell to decide what to do based on types

```haskell
:t (+)
(+) :: Num a => a -> a -> a
```
- What's `Num a =>` ?


## Type Classes

`Num`, `Eq`, `Ord`, and `Show` are `type classes`, and we say that `(==)`, `(<)`, and `(+)` are `type-class polymorphic`

Type classes correspond to set of types which have certain operations defined for them, and type class polyporphic functions work only for types which are instances of the type classes in question. 

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
```
- We can read this as follows: 
  - `Eq` is declared to be a type class with a single parameter, a. 
  - Any type a which wants to be an instance of `Eq` must define two functions, `(==)` and `(/=)`, with the indicated type signatures. 

```haskell
(==) :: Eq a => a -> a -> Bool
```
- The `Eq a` that comes before the `=>` is a `type class constraint`
- In this case, for any type `a`, as long as `a` is an instance of `Eq`, `(==)` can take two values of type `a` and return a `Bool`

If a normal polymorphic type is a promise that the function will work for whatever type the caller chooses, a type class polymorphic function is a restricted promise that the function will work for any type the caller chooses, as long as the chosen type is an instance of the required type classes. 

When `(==)` (or any type class method) is used, the compiler uses type inference to figure out which implementation of `(==)` should be choosen, based on the inferred types of its arguments. 

Type classes can give default implementations of methods in terms of other methods, which should be used whenever an instance does not override the defalut definition with its own. 

```haskell
data Foo = F Int | G Char

instance Eq Foo where
    (F i1) == (F i2) = i1 == i2
    (G c1) == (G c2) = c1 == c2
    _ == _ False
    foo1 /= foo2 = not (foo1 == foo2)
```

### Type Classes and Java Interfaces

Type classes are quite similar to Java interfaces. Both define a set of types/classes which implement a specified list of operations.

There are a couple of important ways in which type classes are more general than Java interfaces:
- When a Java class is defined, any interfaces it implements must be declared. 
- Type class instances, on the other hand, are declared separately from the declaration of the corresponding types, and can even be put in a separate module.
- The types that can be specified for type class methods are more general and flexible than the signatures that can be given for Java interface methods, especially when multi-parameter type classes enter the picture.

### Standard Type Classes

- `Ord`
- `Num`
- `Show`
- `Read`
- `Integral`

### A Type Class Example