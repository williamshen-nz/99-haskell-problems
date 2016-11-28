----------------------------------
-- Ninety-Nine Haskell Problems --
----------------------------------
-- Questions 11 to 20: Lists, continued

-- Problem 11 - Modified run-length encoding
data ListItem a = Single a | Multiple Int a
    deriving (Show)
 
encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
    where
      helper x [] = [(1,x)]
      helper x (y@(a,b):ys)
        | x == b    = (1+a,x):ys
        | otherwise = (1,x):y:ys
 
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode'
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x
      
-- Problem 12 - Decode a run-length encoded list
decodeModified :: [ListItem a]-> [a]
decodeModified = foldl (\x y -> x ++ decodeHelper y) []
    where
        decodeHelper :: ListItem a -> [a]
        decodeHelper (Single x)     = [x]
        decodeHelper (Multiple n x) = replicate n x

-- Problem 13 - Run-length encoding of a list
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = map encodeHelper . encode'
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x
      
-- Problem 14 - Duplicate the elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

dupli' :: [a] -> [a]
dupli' = foldr (\x acc -> x:x:acc) []

-- Problem 15 - Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli [] _ = []
repli [x] n = helper n
   where helper 0 = []
         helper n = x:helper (n-1)
repli (x:xs) n = repli [x] n ++ repli xs n

repli' :: [a] -> Int -> [a]
repli' xs n = foldl (\acc e -> acc ++ repli' e n) [] xs
    where
        repli' _ 0  = []
        repli' x n = x:repli' x (n-1)

-- Problem 16 - Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery list n = helper n list
    where
        helper _ [] = []
        helper num (x:xs)
            | num == 1 = helper n xs
            | otherwise = x : helper (num-1) xs
            
dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n = map fst $ filter (\(x,i) -> i `mod` n /= 0) $ zip xs [1..]
            
-- Problem 17 - Split a list into two parts; the length of the first part is given
split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split [x] _ = ([],[])
split list n = helper n [] list where
    helper 0 xs ys = (reverse xs,ys)
    helper num xs (y:ys) = helper (num-1) (y:xs) ys

split' :: [a] -> Int -> ([a],[a])
split' xs n = (take n xs, drop n xs)

-- Problem 18 - Extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice [x] _ _ = []
slice list start end = helper (start-1) (end-2) list [] where
    helper 0 0 _ acc = reverse acc
    helper 0 e (x:xs) acc = helper 0 (e-1) xs (x:acc)
    helper s e (x:xs) acc = helper (s-1) e xs acc

slice' :: [a] -> Int -> Int -> [a]
slice' xs i k = [x | (x,j) <- zip xs [1..k], i <= j]

-- Problem 19 - Rotate a list N places to the left
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate x 0 = x
rotate x y
  | y > 0 = rotate (tail x ++ [head x]) (y-1)
  | otherwise = rotate (last x : init x) (y+1)

rotate' :: (Enum a) => [a] -> Int -> [a]
rotate' xs n = [(f n) .. last xs] ++ [head xs .. (f (n-1))]
              where f k = xs !! (k `mod` length xs)
              
-- Problem 20 - Remove the K'th element from a list
removeAt :: Int -> [a] -> (a, [a])
removeAt n list = helper (n-1) list [] where
    helper 0 (x:xs) acc = (x, acc ++ xs)
    helper n (x:xs) acc = helper (n-1) xs (x:acc)

removeAt' :: Int -> [a] -> (a, [a])
removeAt' n xs = (xs !! (n-1), take (n-1) xs ++ drop n xs)