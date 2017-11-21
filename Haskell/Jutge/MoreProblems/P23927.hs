import Data.List

sumEquals1 :: Int -> [Int] -> [[Int]]
sumEquals1 s l = filter (< s) l
