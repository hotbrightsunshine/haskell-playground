-- Folding Exercises
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

or' :: [Bool] -> Bool
or' [] = True
or' (x:xs) = x || or' xs

-- foldr :: (a -> b -> b) -> b -> a -> b
andf :: [Bool] -> Bool
andf = foldr (\ a b -> a && b) True

orf :: [Bool] -> Bool
orf = foldr (\ a b -> a || b) False

-- foldr1 :: (a -> a -> a) -> [a] -> a
maximum' :: Ord a => [a] -> a
maximum' xs = foldr1 max  xs

minimum' :: Ord a => [a] -> a
minimum' xs = foldr1 min xs

-- foldl reverse || foldl :: (a -> b -> a) -> a -> [b] -> a 
reverse' ::  [a] -> [a]
reverse' = foldl (\ xs x -> x:xs ) [] 
