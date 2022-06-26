replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' x e = e : replicate' (x-1) e

elemAt :: [a] -> Int -> a
elemAt xs 0 = head xs
elemAt (x:xs) i = elemAt xs (i-1)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] [] = []
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
