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
- Statically typed

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
- Wholemeal programming
