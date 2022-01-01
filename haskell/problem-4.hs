-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 × 99.

-- Find the largest palindrome made from the product of two 3-digit numbers.

-- 55550 -> cannot end in 0
-- range -> 10,000 to 998,001

breakInt :: Integer -> [Integer]
breakInt n
    | n < 10 = [n]
    | otherwise = breakInt (n `quot` 10) ++ [n `mod` 10]

isPalin :: [Integer] -> Bool
isPalin [] = True
isPalin [x] = True
isPalin (x:xs) = x == last xs && isPalin (init xs)

possibleNums :: [[Integer]]
possibleNums = map (breakInt . merge) [(i, j) | i <- [901..999], j <- [901..999], i /= j]
    where merge (x, y) = x * y

mergeIntList :: [Integer] -> Integer
mergeIntList [] = 0
mergeIntList (x:xs) = x * 10 ^ length xs + mergeIntList xs

solution :: Integer
solution = last $ map mergeIntList $ filter isPalin possibleNums

