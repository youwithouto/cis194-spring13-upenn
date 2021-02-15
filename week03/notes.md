# Week 03 | Recursion Patterns, Polymorphism, and the Prelude

> In fact, experienced Haskell programmers hardly ever write recursive functions

- The key is to notice that although functions can theoretically do pretty much anything, in practice there are certain common patterns that come up over and over again. 
  - By abstracting out these patterns into library functions, programmers can leave the low-leve details of actually doing recursion to these functions, and think about problems at a higher level - that's the goal of `wholemeal programming`.

## Recursion Patterns

Operations on lists
- Perform some operation on every element of the list
- Keep only some elements of the list, and throw others away, based on a test
- "Summarise" the elements of the list somehow

### Map
- The things that changes is the operation we want to perform on each element of the list. 

### Filter


### Fold



## Polymorphism
The code would be exactly the same; the only thing that would be different is the type signatures. 

Haskell supports polymorphism for both data types and functions. 
- The word "polymorphism" comes from Greek and means "having many forms": something which is polymorphic works for multiple types. 

### Polymorphic data types

```Haskell
data List t = E | C t (List t)
```

- "t" is a type variable which can stand for nay type
- type variables must start with a lowercase letter, whereas types must start with uppercase
- "List t" means the List type is parameterised by a type, in much the same way that a function can be parameterised by some input



### Polymorphic functions

> The caller of polymorphic functions gets to pick the types





## The Prelude
> The Prelude is a module with a bunch of standard functions that gets implicitly imported into every Haskell program. 

- [Prelude](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html)



## Total and Partial Functions

`head`
- is a `partial function`
  - There are certain inputs for which `head` will crash
  - Functions which have certain inputs that will them recurse infinitely are also called partial

> Functions which are well-defined on all possible inputs are known as `total functions`


> It is good Haskell practice to avoid partial functions as much as possible. 
- Avoiding partial functions is good practice in any programming language. 

Partial Prelude should not be used:
- `head`
- `tail`
- `init`
- `last` 
- `!!`

### Replacing Partial Functions
- Often partial functions like `head`, `tail`, and so on can be replaced by pattern-matching. 

### Writing Partial Functions
Two approaches to write partial functions: 
- Change the output type of the function to indicate the possible failure. 
  - `Maybe`
- When some condition is really guaranteed, then the types ought to reflect the guarantee. 