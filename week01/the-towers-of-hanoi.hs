-- Exercise 5 The Towers of Hanoi
-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

-- 1. move n − 1 discs from a to c using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n − 1 discs from c to b using a as temporary storage.

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n source target temp
  | n <= 0 = []
  | n == 1 = [(source, target)]
  | otherwise = hanoi (n - 1) source temp target ++ hanoi 1 source target temp ++ hanoi (n - 1) temp target source

-- Exercise 6 (Optional) What if there are four pegs instead of three?

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n source target temp1 temp2
  | n <= 0 = []
  | n == 1 = [(source, target)]
  | n == 3 = [(source, temp1), (source, temp2), (source, target), (temp2, target), (temp1, temp2)]
  | otherwise = hanoi4 (n - 1) source temp1 target temp2 ++ hanoi4 1 source target temp1 temp2 ++ hanoi4 (n - 1) temp1 target source temp2