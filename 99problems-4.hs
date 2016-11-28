----------------------------------
-- Ninety-Nine Haskell Problems --
----------------------------------

-- Questions 31 to 41: Arithmetic

-- Problem 31 - Determine whether a given integer number is prime
isPrime :: Int -> Bool
isPrime num = null [x | x <- [2 .. num-1], num `mod` x == 0]

-- Problem 32 - Determine the greatest common divisor of two positive integer numbers
myGCD :: Int -> Int -> Int
myGCD x y
   | y == 0 = x
   | otherwise = myGCD y (x `mod` y)

-- Problem 33 - Determine whether two positive integer numbers are coprime.
coprime :: Int -> Int -> Bool
coprime x y = if myGCD x y == 1 then True else False

-- Problem 34 - Calculate Euler's totient function phi(m).
totient :: Int -> Int
totient n
    | n == 1 = 1
    | otherwise = length [x | x <- [1..n], coprime x n]

-- Problem 35 - Determine the prime factors of a given positive integer. 
--              Construct a flat list containing the prime factors in ascending order.
primeFactors :: Int -> [Int]
primeFactors n = helper n 2
    where
        helper n f
            | f*f > n = [n]
            | n `mod` f == 0 = f:helper (n `div` f) f
            | otherwise = helper n (f+1)

-- Problem 36 - Determine the prime factors of a given positive integer. 
--              Construct a list containing the prime factors and their multiplicity.
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = map swap $ encode $ primeFactors n
  where swap (x,y) = (y,x)
  
encode :: Eq a => [a] -> [(Int,a)]
encode = foldr helper []
    where
      helper x [] = [(1,x)]
      helper x (y@(a,b):ys)
        | x == b    = (1+a,x):ys
        | otherwise = (1,x):y:ys
        
-- Problem 37 - Calculate Euler's totient function phi(m) (improved).
phi :: Int -> Int
phi m = product [(p - 1) * p ^ (c - 1) | (p, c) <- prime_factors_mult m]
-- Problem 38 - N/A

-- Problem 39 - Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
primesR :: Int -> Int -> [Int]
primesR x y = [x | x <- [x..y], isPrime x == True]

-- Problem 40 - Goldbach's Conjecture
-- Problem 41 - Goldbach composition, even numbers