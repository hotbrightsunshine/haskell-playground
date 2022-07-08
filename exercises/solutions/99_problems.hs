{- 
    
██╗░░░░░██╗░██████╗████████╗░██████╗  ██╗
██║░░░░░██║██╔════╝╚══██╔══╝██╔════╝  ██║
██║░░░░░██║╚█████╗░░░░██║░░░╚█████╗░  ██║
██║░░░░░██║░╚═══██╗░░░██║░░░░╚═══██╗  ██║
███████╗██║██████╔╝░░░██║░░░██████╔╝  ██║
╚══════╝╚═╝╚═════╝░░░░╚═╝░░░╚═════╝░  ╚═╝

-}

last99 :: [a] -> Maybe a
last99 (x:xs) = if null xs then Just x else last99 xs
last99 [] = Nothing

lastButOne99 :: [a] -> Maybe a
lastButOne99 (x:xs) = if length xs == 1 then Just x else last99 xs
lastButOne99 [] = Nothing

kthElement99 :: Int -> [a] -> Maybe a
kthElement99 k (x:xs) = if k == 0 then Just x else kthElement99 (k-1) xs
kthElement99 k [] = Nothing

length99 :: [a] -> Int
length99 = foldr (\ x acc -> acc + 1 ) 0

reverse99 :: [a] -> [a]
reverse99 = foldl (flip (:)) []

palindrome99 :: (Eq a) => [a] -> Bool
palindrome99 xs = xs == reverse99 xs

removeDupl99 :: (Eq a) => [a] -> [a]
removeDupl99 = foldl (flip suppl) [] where
    suppl x acc 
        | null acc = [x]
        | x == head acc = acc
        | otherwise = x : acc

group99 :: (Eq a) => [a] -> [[a]]
group99 = foldl (flip suppl) [] where 
    suppl x acc 
        | null acc = [[x]]
        | x == head acc = [x] : [first acc]


