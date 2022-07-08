-- 8 Luglio 2022 

module Main where
import Data.Complex
import Data.List

groupIn :: Int -> [a] -> [[a]]
groupIn _ [] = []
groupIn n xs
    | n > 0 = take n xs : groupIn n (drop n xs)
    | otherwise = error "Non si fa!"

truncate' :: Double -> Int -> Double
truncate' x n = fromIntegral (floor (x * t)) / t
    where t = 10^n

range' :: Double -> Double -> Double -> [Double]
range' start end incr
    | start <= end = truncate' start 2 : range' (start+incr) end incr
    | otherwise = []

f :: Complex Double -> Complex Double -> Complex Double
f z c = z' + c
    where z' = z * z

fArr :: Complex Double -> Complex Double -> [Complex Double]
fArr z c = res : fArr res c
    where res = f z c

-- To improve...
fArrFinite :: Int -> Complex Double -> Complex Double -> [Complex Double]
fArrFinite threshold z c = take threshold $ fArr z c

doesDiverge :: [Complex Double] -> Bool
doesDiverge = any (\ (a :+ b) -> isInfinite a || isInfinite b)

doesArrDiverge :: Int -> Complex Double -> Bool
doesArrDiverge threshold c = doesDiverge $ fArrFinite threshold 0 c

mandelbrot :: Double -> Double -> [Complex Double]
mandelbrot extreme prec = [  (r :+ i) | i <- range' (-extreme) extreme prec, r <-  range' (-extreme) extreme prec ]

boolToString :: Bool -> Char
boolToString True = ' '
boolToString False = '█'

showable :: String -> String
showable str = intercalate "\n" $ groupIn (round row_len) str
    where row_len = sqrt (fromIntegral $ length str)

render :: Int -> [Complex Double] -> String
render threshold xs  = showable $ map (boolToString . doesArrDiverge threshold) xs

main :: IO ()
main = do
    let thrsld = 100 :: Int 
    let scale = 1.5 :: Double 
    let precision = 0.05 :: Double 
    putStrLn $ render thrsld $ mandelbrot scale precision