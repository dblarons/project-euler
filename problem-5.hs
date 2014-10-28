-- 2520 is the smallest number that can be divided by each 
-- of the numbers from 1 to 10 without any remainder.

-- What is the smallest positive number that is evenly 
-- divisible by all of the numbers from 1 to 20?

-- http://learnxinyminutes.com/docs/haskell/

dividesList :: Integer -> [Integer] -> Bool
dividesList x = all (\y -> x `mod` y == 0)

-- only try nums that are multiples of the last three nums times each other
findSmallest :: [Integer] -> Integer
findSmallest xs = head [i | i <- [jump xs, 2 * jump xs..], dividesList i xs]
    where jump xs = (last . init . init) xs * (last . init) xs * last xs

-- remove numbers that are factors of a larger number in the array
removeDups :: [Integer] -> [Integer]
removeDups [] = []
removeDups (x:xs)
    | isCommon x xs = removeDups xs
    | not $ isCommon x xs = x : removeDups xs
    where isCommon x = any (\y -> y `mod` x == 0)

solution :: Integer
solution = findSmallest $ removeDups [1..20]

