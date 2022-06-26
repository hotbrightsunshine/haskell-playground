-- Esercizio 1 - Scrivi le versioni "sicure" delle funzioni head, tail, init e last
safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead [] = Nothing

safeTail :: [a] -> Maybe [a]
safeTail xs
  | null xs = Nothing
  | otherwise  = Just $ drop 1 xs


safeLast :: [a] -> Maybe a
safeLast xs = safeHead $ reverse xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeinit [_] = Nothing
safeinit xs = Just $ init xs

splitWith :: (a -> Bool) ->  [a] -> [[a]]
splitWith func xs = previous : remaining where
  
splitWith _ [] = []
