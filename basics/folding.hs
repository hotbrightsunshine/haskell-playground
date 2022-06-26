addToTail :: [a] -> a -> [a]
addToTail xs x = x : xs

rev' :: [a] -> [a]
rev' = foldl (\ acc x -> x : acc) []

