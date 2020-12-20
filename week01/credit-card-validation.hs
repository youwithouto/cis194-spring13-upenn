-- Validating Credit Card Numbers

-- Exercise 1
-- We need to first find the digits of a number.
-- toDigits should convert positive Integers to a list of digits. (For 0 or negative inputs, toDigits should return the empty list.)
toDigits :: Integer -> [Integer]
toDigits num = reverse $ toDigitsRev num

-- toDigitsRev should do the same, but with the digits reversed.
toDigitsRev :: Integer -> [Integer]
toDigitsRev num
  | num <= 0 = []
  | num < 10 = [num]
  | otherwise = (num `mod` 10) : toDigitsRev (num `div` 10)

-- The second case above can be omitted, as it is coverd by the third case (the code is left as-is for future reference)

-- Exercise 2
-- Once we have the digits in the proper order, we need to double every other one.
-- doubleEveryOther should double every other number beginning from the right, that is, the second-to-last, fourth-to-last, . . . numbers are doubled.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther nums = reverse $ doubleEveryOtherReverse $ reverse nums

doubleEveryOtherReverse :: [Integer] -> [Integer]
doubleEveryOtherReverse [] = []
doubleEveryOtherReverse [a] = [a]
doubleEveryOtherReverse (a : (b : c)) = a : 2 * b : doubleEveryOtherReverse c

-- Exercise 3
-- The output of doubleEveryOther has a mix of one-digit and two-digit numbers.
sumDigits :: [Integer] -> Integer
sumDigits nums = sum $ map (sum . toDigits) nums

-- Exercise 4
-- indicates whether an Integer could be a valid credit card number
-- Example: validate 4012888888881881 = True
-- Example: validate 4012888888881882 = False
validate :: Integer -> Bool
validate num
  | mod (sumDigits $ doubleEveryOther $ toDigits num) 10 == 0 = True
  | otherwise = False
