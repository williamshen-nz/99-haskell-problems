----------------------------------
-- Ninety-Nine Haskell Problems --
----------------------------------
-- Questions 1 to 10: Lists

-- Problem 1 - find the last element of a list
myLast :: [a] -> a
myLast list = case list of
   [] -> error "Empty list."
   [x] -> x
   (x:xs) -> myLast xs

myLast' :: [a] -> a
myLast' = head . reverse

-- Problem 2 - find the last but one element of a list
myButLast :: [a] -> a
myButLast list = reverse list !! 1

myButLast' :: [a] -> a
myButLast' = head . tail . reverse

-- Problem 3 - find the K'th element of a list, first element in the list is number 1
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt [] _ = error "Index out of bounds"
elementAt (_:xs) k
  | k < 1 = error "Index out of bounds"
  | otherwise = elementAt xs (k - 1)
  
elementAt' :: [a] -> Int -> a
elementAt' list n = list !! (n-1)
  
-- Problem 4 - find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = foldr (\_ n -> n + 1) 0

-- Problem 5 - reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (\acc x -> x:acc) []

-- Problem 6 - check whether a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = myReverse list == list

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = foldl (\acc (a,b) -> if a == b then acc else False) True input
    where input = zip xs (reverse xs)
    
-- Problem 7 - flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List xs) = foldr (++) [] $ map flatten' xs

-- Problem 8 - eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress list = compress_acc list []
   where 
      compress_acc [] acc = acc
      compress_acc [x] acc = (acc ++ [x])
      compress_acc (x:xs) acc
        | x == (head xs)  = compress_acc xs acc
        | otherwise       = compress_acc xs (acc ++ [x])
        
compress' :: (Eq a) => [a] -> [a]
compress' = foldr func []
    where
        func x [] = [x]
        func x acc
           | x == head acc = acc
           | otherwise = x:acc

-- Problem 9 - pack consecutive duplicates of list elements into sublists
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)
              
pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (x:takeWhile (==x) xs):pack'(dropWhile (==x) xs)

-- Problem 10 - run length encoding of list
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = encoder' 1 x xs where
    encoder' n x [] = [(n,x)]
    encoder' n x (y:ys)
       | x == y = encoder' (n+1) x ys
       | otherwise = (n, x):encoder' 1 y ys
       
encode' :: (Eq a) => [a] -> [(Int, a)]
encode' [] = []
encode' (x:xs) = (length $ x:takeWhile (==x) xs, x):encode(dropWhile (==x) xs)