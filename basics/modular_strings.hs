evolve_ :: [Int] -> Int -> Int -> Int -> [Int]
evolve_ [] _ _ _ = []
evolve_ (x:xs) n i first
    | (i+1 == n)    = ((x) `mod` n) : []
    | (i == 0)      = (x + (head xs)) `mod` n : evolve_ xs n (i+1) (head xs)
    | otherwise     = (x + (head xs)) `mod` n : evolve_ xs n (i+1) first

evolve :: [Int] -> [Int]
evolve xs = evolve_ xs (length xs) 0 0

evolveLoop :: [Int] -> [[Int]]
evolveLoop xs = evolve xs : evolveLoop (evolve xs)

primordial :: Int -> [Int]
primordial n = take (n-1) (repeat 0) ++ [1]


