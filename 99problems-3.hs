----------------------------------
-- Ninety-Nine Haskell Problems --
----------------------------------

-- Questions 21 to 28: Lists again

-- Problem 21 - Insert an element at a given position into a list
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = take (n-1) xs ++ [x] ++ drop (n-1) xs

insertAt' :: a -> [a] -> Int -> [a]
insertAt' x ys 1 = x:ys
insertAt' x (y:ys) n = y:insertAt x ys (n-1)

-- Problem 22 - Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range x y = [x..y]

-- Problem 23 - Extract a given number of randomly selected elements from a list.
-- Problem 24 - Lotto: Draw N different random numbers from the set 1..M.
-- Problem 25 - Generate a random permutation of the elements of a list.

-- Problem 26 - Generate the combinations of K distinct objects chosen from the N elements of a list
-- Problem 27 - Group the elements of a set into disjoint subsets.
-- Problem 28 - Sorting a list of lists according to length of sublists
