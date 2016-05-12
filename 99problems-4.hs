----------------------------------
-- Ninety-Nine Haskell Problems --
----------------------------------

-- Questions 31 to 41: Arithmetic

-- Problem 31 - Determine whether a given integer number is prime
isPrime :: Int -> Bool
isPrime num = null [x | x <- [2 .. num-1], num `mod` x == 0]

-- Problem 32 - Determine the greatest common divisor of two positive integer numbers
-- Problem 33 - Determine whether two positive integer numbers are coprime. 
-- Problem 34 - Calculate Euler's totient function phi(m).
-- Problem 35 - Determine the prime factors of a given positive integer. 
--              Construct a flat list containing the prime factors in ascending order.
-- Problem 36 - Determine the prime factors of a given positive integer. 
--              Construct a list containing the prime factors and their multiplicity.
-- Problem 37 - Calculate Euler's totient function phi(m) (improved).
-- Problem 38 - N/A

-- Problem 39 - Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
primesR :: Int -> Int -> [Int]
primesR x y = [x | x <- [x..y], isPrime x == True]