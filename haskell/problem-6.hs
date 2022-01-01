-- The sum of the squares of the first ten natural numbers is,
-- 1^2 + 2^2 + ... + 10^2 = 385

-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)^2 = 55^2 = 3025

-- Hence the difference between the sum of the squares of the first 
-- ten natural numbers and the square of the sum is 3025 − 385 = 2640.

-- Find the difference between the sum of the squares of the first 
-- one hundred natural numbers and the square of the sum.

solution :: Integer
solution = (sum [1..100] ^ 2) - sum (map (^2) [1..100])

-- I like this solution because it reads like English. After
-- reading the comments on Project Euler, I (re-)discovered that
-- I could have used polynomials to solve the problem, but doing
-- so would have obfuscated the meaning slightly. Though the 
-- solution certainly would have been faster to compute.