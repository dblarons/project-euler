-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

import Data.Numbers.Primes

primeFactor :: Integer -> [Integer]
primeFactor n = filter (\x -> n `mod` x == 0) $ takeWhile (< (n `quot` 2)) primes


solution :: [Integer]
solution = primeFactor 600851475143

-- Bad solution: This gives the right answer, but doesn't finish.
