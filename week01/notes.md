# Week 01 | Haskell Basics

## What is Haskell?

Haskell is a lazy, functional programming language created in the 1980's by a committee of academics. 

- Functional
  - There is no precise, accepted meaning for the term "functional"
  - When we say that Haskell is functional, we usually have in mind two things:
    - Functions are first-class, that is, functions are values which can be used in exactly the same ways as any other sort of value.
    - The meaning of Haskell programs is centered around evaluating expressions rather than exectuing instructions.
- Pure
  - Haskell expressions are always referentially trasparent:
    - No mutation! Everything is immutable.
    - Expressions never have "side effects".
    - Calling the same function with the same arguments results in the same output everytime. 
  - Benefits:   
    - Equational reasoning and refactoring: In Haskell, one can always "replace equals by equals"
    - Parallelism: Evaluating expressions in parallel is easy when they are guaranteed not to affect one another
    - Fewer headaches: Simply put, unrestricted effects and action-at-a-distance makes for programs that are hard to debug, maintain, and reason about.
- Lazy
  - In Haskell, expressions are not evaluated until their results are actually needed. 
  - Benefits:
    - It's easy to define a new control structure just by defining a function
    - It's possible to define and work with infinite data structures
  - Downside:
    - Reasoning about time and space usage becomes much more complicated. 
- Statically typed
  - Types are all checked at compile-time
  - Programs with types errors will not even compile, much less run.

## Themes

Throughout this course, we will focus on three main themes.

- Types
  - Helps clarify thinking and express program structure
    - The first step in writing a Haskell program is usually to write down all the types. 
  - Serves as a form of documentation
    - Nothing should be duplicated
    - Taking similar pieces of code and factoring out their commonality is known as the process of abstraction. 
  - Turns runtime errors into compile-time errors
- Abstraction
  - "Don't Repeat Yourself" is a mantra often heard in the world of programming. 
    - Also known as the "Abstraction Principle"
    - The idea is that nothing should be duplicated:
      - every idea, algorithm, and piece of data should occur exactly once in your code
  - Taking similar pieces of code and factoring out their commonality is known as the process of `abstraction`
- Wholemeal programming
  - `Wholemeal programming` -- coined by Geraint Jones
    - means to think big
      - Work with an entire list, rather than a sequence of elements
      - Develop a solution space, rather than an individual solution
      - Imagine a graph, rather than a single path
    - Often complemented by the idea of projective programming
      - First solve a more general problem, then extract the interesting bits and pieces by transforming the general program into more specialised ones
    - Functional languages excel at wholemeal programming



## Literal Haskell

- Only lines preceded by `>` and a space are code; everything else is a comment
- Literate Haskell documents have an extension of `.lhs`, whereas non-literate Haskell source files use `.hs`



## Declarations and variables







## Basic Types





## GHCi







## Arithmetic







## Boolean logic









## Defining basic functions









## Pairs







## Using functions, and multiple arguments







## Lists







## Constructing lists







## Functions on lists







## Combining functions

> It's good Haskell style to build up more complex functions by combining many simple ones.





## A word about error messages