# Week 02 | Algebraic Data Types

## Thoughts

- "Haskell programmers start working on a project by defining the types", this is actually the process of creating abstractions. In the case of the [Homework](./02-ADTs.pdf), three types are defined first and functions need to be created for those types. If I were solving the same problems with an imperative language, I guess the first thing I do would be defining the functions for each of the types. 

## Useful Prelude Functions

- `read`
- `lines`
- `words`
- `unwords`
- `take`
- `drop`
- `.`

## Enumeration Types
Like many programming languages, Haskell allows programmers to create their own `enumeration types`. 

```Haskell
data Thing = Shoe 
           | Ship 
           | SealingWax 
           | Cabbage 
           | King
  deriving Show
```
- This declares a new type called Thing with five `data constructors`, i.e., Shoe, Ship, etc. which are the only values of type Thing. 

## Beyond Enumerations

`Enumerations` are only a special case of Haskell's more general `algebraic data types`. 

```Haskell
data FailableDouble = Failure
                    | OK Double
  deriving Show
```
- The FailableDouble type has two `data constructors`. 
  - The first one, Failure, takes no arguments, so Failure by itself is a value of type FailableDouble
  - The second one, OK, takes an arguemnt of type Double. 
    - OK by itself is not a value of type FailableDouble; we need to give it a Double. 

Data constructors can have more than one argument. 

```Haskell
-- Store a person's name, age, and favourite Thing.
data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan  = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a
```
- The type constructor and data constructor are both named Person, but they inhabit different namespaces and are different things. 
- This idiom, giving the type and data constructor of a one-constructor type the same name, is common, but can be confusing until you get used to it. 


## Algebraic Data Types in General

> In general, an algebraic data type has one or more data constructors, and each data constructor can have zero or more arguments. 

Type and data constructor names must always start with a capital letter; variables (including names of functions) must always start with a lowercase letter. 

- Otherwise, Haskell parsers would have quite a difficult job figuring out which names represent variables and which represent constructors. 

## Pattern-Matching
> Fundamentally, pattern-matching is about taking apart a value by finding out which constructor it was built with. 

- This information can be used as the basis for deciding what to do, in deed, in Haskell, this is the only way to make a decision. 

- Parentheses are required around patterns consisting of more than just a single constructor. 
- An underscore `_` can be used as a "wildcard pattern" which matches anything
- A pattern of the form x@pat can be used to match a value against the pattern pat, but also give the name x to the entire value being matched
- Patterns can be nested. 
- Literal values like 2 or 'c' can be thought of as constructors with no arguments
  - We can pattern-match against literal values

```Haskell
pat ::= _
     |  var
     |  var @ ( pat )
     |  ( Constructor pat1 pat2 ... patn )
```
- An underscore is a pattern. 
- A variable by itself is a pattern.
- @-patterns
- A constructor name followed by a sequence of patterns is itself a pattern.


## Case Expressions
The fundamental construct for doing pattern-matching in Haskell is the `case expression`. 
In general, a case expression lookes like:

```Haskell
case exp of
  pat1 -> exp1
  pat2 -> exp2
  ...
```
- When evaluated, the expression exp is matched against each of the patterns pat1, pat2, ... in turn. 
  - The first matching pattern is chosen, and the entire case expression evaluates to the expression corresponding to the matching pattern. 
- The syntax for defining functions we have seen is really just convenient syntax sugar for defining a case expression. 

```Haskell
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d    -> d

-- vs. 
failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d
```


## Recursive Data Types
>  Data types can be `recursive`, that is, defined in terms of themselves. 

- The type of lists
  - A list is either empty, or a single element followed by a remaining list. 
- The type of binary tree
  - We can define a type of binary trees with an Int value stored at each internal node, and a Char stored at each leaf

```Haskell
data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
```

> We often use recursive functions to process recursive data types