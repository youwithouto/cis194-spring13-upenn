# Week 06 | Lazy Evaluation

Haskell is lazy

## Strict Evaluation

`lazy evaluation` <=> `strict evaluation`

- Under a strict evaluation strategy, function arguments are completely evaluated before passing them to the function
- The benefit of strict evaluation is that it is easy to predict when and in what order things will happen. 
  - Usually languages with strict evaluation will even specify the order in which function arguments should be evaluated. 

## Side Effects and Pruity

> What's really at issue here is the presence or absence of side effects
- By `side effect` we mean anything that causes evaluation of an expression to interact with something outside itself
  - The root issue is that such outside interactions are time-sensitive
    - Modifying a global variable
    - Printing to the screen
    - Reading from a file or the network

> Lazy evaluation makes it hard to reason about when things will be evaluated; hence including side effects in a lazy language would be extremely unintuitive. 
- This is the reason Haskell is pure
  - Initially, the designers of Haskell wanted to make a lzay functional language, and quickly realised it would be impossilbe unless it also disallowed side effects
  - But A language with no side effects would not be very useful 
    - You would not be able to get any input from ther user, or print anything to the screen, or read from a file.
  - The challenge facing the Haskell designers was to come up with a way to allow such effects in a principled, restricted way that did not interfere with the essential purity of the language. 
  - They finally did come up with something, namely `IO monad`. 


## Lazy Evaluation

- Under a `lazy evaluation` strategy, **evaluation of function arguments is delayed as long as possible**: 
  - they are not evaluated until it actually becomes necessary to do so.
    - When some expression is given as an argument to a function, it is simply packaged up as an unevaluated expression, called a `thunk`, without doing any actual work.
      - If the thunk turns out being unused, it will be thrown away by the garbage collector.
      - Thunks are evaluted only enough to allow a pattern match to proceed, and no further

## Pattern Matching Drives Evaluation

> `Pattern matching` drives evaluation; expressions are only evaluated when pattern-matched.

So far, we agreed that whether a function used its arguments determines if the arguments are evaluated, but this is actually not the most direct factor for evaluation. 

- A function can use its arguments but it does not need to know anything about it. 

> Thunks are evaluated only enough to allow a pattern match to proceed and no further. 

GHC uses a technique called `graph reduction`
- the expression being evaluated is actually represented as a graph, so that different parts of the expression can share pointers to the same subexpression. 
  - this ensures that work is not duplicated unnecessarily


## Consequences

Laziness has some very interesting, pervasive, and nonobvious consequences.

### Purity

> Choosing a lazy evaluation strategy essentially forces you to also choose purity. 


### Understanding Space Usage

When using laziness, sometimes it becomes tricky to reason about the space usage of your programs.

```Haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z 		   = z
foldl f z (x:xs) = foldl f (f z x) xs

--
foldl (+) 0 [1, 2, 3] = 
foldl (+) (0 + 1) [2, 3] = 
foldl (+) ((0 + 1) + 2) [3] = 
foldl (+) (((0 + 1) + 2) + 3) [] = 
(((0 + 1) + 2) + 3) = 
((1 + 2) + 3) = 
(3 + 3) = 
6
```

- Since the value of the accumulator is not demanded until recursing through the entire list, the accumulator simply builds up a big unevaluated expression (((0 + 1) + 2) + 3), which finally gets reduced to a value at the end
  - Two problems:
    - Inefficient
      - There is no point in transferring all the numbers from the list into a different list-like thing (the accumulator thunk) before actually adding them up
    - A more subtle, more insidious problem
      - evaluating the expression (((0 + 1) + 2) + 3) actually requires push the 3 and 2 onto a stack before being able to compute 0 + 1 and then unwinding the stack, adding along the way. 
        - This is not a problem for small examples, but for very long lists it's a big problem. 
          - There is usually not as much space available for the stack, so this can lead to a stack overflow. 

`foldl` vs. `foldl'`

- The solution to the above problem is to use `foldl'` instead of `foldl`
- `foldl'` requires its second argument (the accumulator) to be evaluated before it proceeds, so a large thunk never builts up

### Short-Circuiting Operators

Short-circuiting behaviour of `&&` and `||` is a special exception to the usual strict semantics of the language. 

In Haskell, we can define short-circuiting operators without any special cases. 

- `&&` and `||` are just plain old library functions


### User-defined Control Structures

> In Haskell, we can define our own `control structures`. 

- `if` doesn't get used that much in Haskell
- in most situations, we prefer `pattern-matching` or `guards`
- and more with `monads`


### Infinite Data Structures

Lazy evaluation also means that we can work with `infinite data structures`. 

Defining an infinite data structure actually only creates a thunk , which we can think of as a "seed" out of which the entire data structure can potentially grow, depending on what parts actually are used/needed. 

Another practical application area is "effectively infinite" data structures, such as the trees that might arise as the state space of a game. 
- Although tree is finite in theory, it is so large as to be effectively infinite -- it certainly would not fit in memory. 
- Using Haskell, we can define the tree of all possible moves, and then write a seeparate algorithm to explore the tree in whatever way we want. 
  - Only the parts of the tree which are actually explored will be computed. 


### Pipelining/Wholemeal Programming

>  Doing "pipelined" incremental transformations of a large data structure can actually be memory-efficient. 

- Due to laziness, each stage of the pipeline can operate in lockstep, only generating each bit of the result as it is demanded by the next stage in the pipeline. 


### Dynamic Programming

Usually, one must take great care to fill in entries of a `dynamic programming` table in the proper order, so that every time we compute the value of a cell, its dependencies have already been computed. 
- If we get the order wrong, we get bogus results. 

Using lazy evaluation we can get the Haskell runtime to work out the proper order of evaluation for us. 

```haskell
import Data.Array

knapsack01 :: [Double]   -- values 
           -> [Integer]  -- nonnegative weights
           -> Integer    -- knapsack size
           -> Double     -- max possible value
knapsack01 vs ws maxW = m!(numItems-1, maxW)
  where numItems = length vs
        m = array ((-1,0), (numItems-1, maxW)) $
              [((-1,w), 0) | w <- [0 .. maxW]] ++
              [((i,0), 0) | i <- [0 .. numItems-1]] ++
              [((i,w), best) 
                  | i <- [0 .. numItems-1]
                  , w <- [1 .. maxW]
                  , let best
                          | ws!!i > w  = m!(i-1, w)
                          | otherwise = max (m!(i-1, w)) 
                                            (m!(i-1, w - ws!!i) + vs!!i)
              ]

example = knapsack01 [3,4,5,8,10] [2,3,4,5,9] 20
```