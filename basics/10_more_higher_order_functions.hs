import Data.Char (toLower)
quicksort :: Ord a => (a -> a -> Ordering) -> [a] -> [a]
quicksort f [] = []
quicksort f (x:xs) = (quicksort f bef) ++ (x : eq) ++ (quicksort f aft)
    where
        bef = filter (\ y -> y `f` x == LT ) xs
        aft = filter (\ y -> y `f` x == EQ) xs
        eq  = filter (\ y -> y `f` x == GT) xs

insensitive :: Char -> Char -> Ordering
insensitive x y = toLower x `compare` toLower  y

for :: (a) -> (a -> Bool) -> (a -> a) -> (a -> IO () ) -> IO ()
for init cond job action = 
    if cond init 
        then do
            action init
            for (job init) cond job action
        else
            return ()
