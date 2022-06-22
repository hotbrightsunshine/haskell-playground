{-
    Define a function productList :: [Int] → Int which returns the product of a list
    of integers. You should take the product of the empty list to be 1.
-}

productList :: [Int] -> Int
productList [] = 1
productList (x:xs) = x * productList xs

{-
    Define a function myand :: [Bool ] → Bool which returns the conjunction of a
    list. 
-}

myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand xs


{-
    Define a function concatList :: [[Int]] → [Int] which flattens a list of lists of
    integers into a single list of integers
-}

concatList :: [[Int]] -> [Int]
concatList [] = []
concatList [[]] = []

concatList (x:xs) = x ++ concatList xs

-- This function counts how many times a number n occurs in a list x:xs
count :: [Int] -> Int -> Int
count [] _ = 0;
count (x:xs) n
    | (n == x) = 1 + count xs n
    | otherwise = count xs n

sum' a b = a + b 

pari :: Int -> Bool
pari n
  | n `mod` 2 == 0 = True
  | otherwise = False
