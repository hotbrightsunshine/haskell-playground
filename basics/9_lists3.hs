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


-- Scan Exercises
-- scanl (b -> a -> b) b ([a])
-- scanr (a -> b -> b) b ([a])

factorials :: Integer -> [Integer]
factorials n = scanl1 (*) [1..n]

-- List comprehension

divisibleBy d xs = [ n | n <- xs, n `mod` d == 0]

tailsOf xs = [ tail t | t <- xs,  not (null t), head t > 5]

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter pred xs = [ i | i <- xs, pred i ]

mymap :: (t -> a) -> [t] -> [a]
mymap f xs = [f t | t <- xs]

doubleOfFirstForEvenSeconds :: [(Int, Int)] -> [Int]
doubleOfFirstForEvenSeconds ps = map (\ (x, _) -> x * 2 ) $ filter (\ (_, y) -> even y) ps

