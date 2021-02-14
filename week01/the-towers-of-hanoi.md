# The Towers of Hanoi

Background:

- The Towers of Hanoi is a classic puzzle with a solution that can be described recursively. 

Related Haskell concepts:

- Type synonym
- Tuple construction 
- Recusion



### Exercise 5

Target:

- Given the number of discs and names for the three pegs, Hanoi should return a list of moves to be performed to move the stack of discs from the first peg to the second.

Example:

```Haskell
hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]
```

Solution:

To move n discs (stacked in increasing size) from peg a to peg b using peg c as temporary storage:

1. move n - 1 discs from a to c using b as temporary storage
2. move the top disc from a to b
3. move n - 1 discs from c to b using a as temporary storage

```Haskell
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n source target temp 
| n <= 0    = []
| n == 1    = [(source, target)]
| otherwise = hanoi (n - 1) source temp target ++ hanoi 1 source target temp ++ hanoi (n - 1) temp target source
```



### Exercise 6 (TODO)

Target:

- What if there are four pegs instead of three?

