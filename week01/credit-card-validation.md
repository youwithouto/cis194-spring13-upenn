# Validating Credit Care Numbers

Backgournd: 

- Most credit providers rely on a checksum formula for distinguishing valid numbers from random collections of digits (or typing mistakes)

Task: 

- Implement the validation algorithm

Related Haskell concepts:

- Function definition
- Function with guards
- Function composition (`.` and `$`)
- Point-free style
- List construction
- `mod` and `div`



### Exercise 1

Target: 

- Find the digits of a number

Assumptions:

- The functions should be able to handle both positive and negative Integers

Definition:

- Convert positive Integers to a list of digits (and reverse)

```haskell
toDigits    :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
```

Examples:

```haskell
toDigits 1234    == [1, 2, 3, 4]
toDigitsRev 1234 == [4, 3, 2, 1]
toDigits 0		   == []
toDigits (-17)   == []  -- just return an empty array, []
```

Solution:

```haskell
toDigits :: Integer -> [Integer]
toDigits nums = reverse . toDigitsRev $ nums
toDigits = reverse . toDigitsRev -- point-free style

toDigitsRev :: Integer -> [Integer]
toDigitsRev num 
| num <= 0  = []
| otherwise = (num `mod` 10) : toDigitsRev (num `div` 10)
```



### Exercise 2

Target:

- Double every other digit (from the right) in a list

Definition:

```haskell
doubleEveryOther :: [Integer] -> [Integer]
```

Examples:

```haskell
doubleEveryOther [8, 7, 6, 5] == [16, 7, 12, 5]
doubleEveryOther [1, 2, 3]    == [1, 4, 3]
```

Solution:

```haskell
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther nums = reverse . doubleEveryOtherRev $ nums
doubleEveryOtherRev = reverse . doubleEveryOtherRev -- point-free style

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [a] = [a]
doubleEveryOtherRev (a : b: c) = a : (2 * b) : doubleEveryOtherRev c
```



### Exercise 3

Target:

- The output of `doubleEveryOther` has a mix of one-digit and two-digit numbers. Calculate the sum of all digits

Definition:

```haskell
sumDigits :: [Integer] -> Integer
```

Examples:

```haskell
sumDigits [16, 7, 12, 5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
```

Solution:

```haskell
sumDigits :: [Integer] -> Integer
sumDigits nums = sum $ map (sum . toDigits) nums
sumDigits = sum $ map (sum . toDigits) -- point-free style
```



### Exercise 4

Target:

- Define a function that indicates whether an Integer could be a valid credit card number
  - Calculate the remainder when the sum is dividied by 10
  - If the result equals 0, then the number is valid

Definition:

```haskell
validate :: Integer -> Bool
```

Examples:

```Haskell
validate 4012888888881881 = True
validate 4012888888881882 = False
```

Solution:

```Haskell
validate :: Integer -> Bool
validate num = (sumDigits . doubleEveryOther . toDigits $ num) mod 10 == 0
```

