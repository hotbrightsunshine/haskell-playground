addToTail :: [a] -> a -> [a]
addToTail xs x = x : xs

rev' :: [a] -> [a]
rev' = foldl (\ acc x -> x : acc) []

-- Correct
sumsq :: [Int] -> Int
sumsq = foldr (\ x y -> x^2 + y) 0

length' :: [Int] -> Int
length' = foldr (\ x acc -> acc + 1 ) 0

lengthl :: [Int] -> Int
lengthl = foldl (\ acc x -> acc + 1 ) 0
