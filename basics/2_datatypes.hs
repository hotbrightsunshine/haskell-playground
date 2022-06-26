-- lastButOne
lastButOne :: [a] -> a
lastButOne xs
  | length xs == 1 = head xs
  | otherwise = last ( init xs )

-- Write the converse of fromList for the List type: a function that takes a List a and generates a [a].

data List a = Cons a (List a) | Nil
              deriving (Show)

fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

fromList' :: List a -> [a]
fromList' (Cons x xs) = x : fromList' xs
fromList' Nil = []

-- Esercizio 1 - length'
length' (x:xs) = 1 + length' xs
length' [] = 0

-- Esercizio 3 - media di una lista
media xs = sum xs / fromIntegral (length' xs) 

-- Esercizio 4 - lista palindroma
palindrome :: [a] -> [a]
palindrome xs = xs ++ (reverse xs)

-- Esercizio 5 -- verificare che sia palindromo
isPalindrome xs = xs == reverse xs


-- Esercizio 7 -- join function
intersperse' :: a -> [[a]] -> [a]
intersperse' e (x:xs)
  | length xs >= 1 = x ++ (e : []) ++ intersperse' e xs
  | otherwise     = x
intersperse' e [] = []

-- Esercizio 8 -- Altezza di un albero binario

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

heightOf :: Tree t -> Int
heightOf Empty = 0
heightOf (Node a Empty Empty) = 1
heightOf (Node a t1 Empty)  = 1 + heightOf t1
heightOf (Node a Empty t1) = 1 + heightOf t1
heightOf (Node a t1 t2) = 1 + maximum [ heightOf t1, heightOf t2 ]

-- Esercizio 9 -- Segmenti e direzioni

data Direction = ToLeft | ToRight | StraightLine deriving (Show)
data Point = Point { xCoord::Float , yCoord::Float } deriving (Show)

inclination :: Point -> Point -> Float
inclination p b
  | xCoord b - xCoord p == 0 = error "Divition by 0"
  | otherwise = (yCoord b - yCoord p) / (xCoord b - xCoord p)

direction :: Point -> Point -> Point -> Direction
direction p0 p1 p2
  | inclination p0 p1 > inclination p1 p2 = ToRight
  | inclination p0 p1 < inclination p1 p2 = ToLeft
  | otherwise = StraightLine

-- Examples
p0 = Point 2.3 2.3
p1 = Point 2.6 2.6
p2 = Point 4.8 0
p3 = Point 4.81 6.3

p4 = Point 5 5
p5 = Point 6 6
p6 = Point 7 7

pointList = [p0, p1, p2, p3, p4, p5, p6]

-- Esercizio 10 -- Lista di Directions data una lista di punti
directionList :: [Point] -> [Direction]
directionList (x:y:z:xs) = direction x y z : directionList (y:z:xs)
directionList (x:xs) = []
directionList _ = []
