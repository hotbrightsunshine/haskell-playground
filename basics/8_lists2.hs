import Data.List

-- Map Exercises
divisorsArr :: [Int] -> [[Int]]
divisorsArr [] =  [[]]
divisorsArr xs = map (divisors) xs 

divisors p = [ f | f <- [1..p], p `mod` f == 0 ]

-- Run Length Encoding
rlencode :: [Char] -> [(Int, Char)]
rlencode [] = []
rlencode xs = map (\ arr -> (length arr, head arr)) $ group xs
