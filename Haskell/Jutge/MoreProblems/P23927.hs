import Data.List

sumEquals1 :: Int -> [Int] -> [[Int]]
sumEquals1 0 l =

sumEquals1' :: Int -> [Int] -> [[Int]]
sumEquals1' s (e:l) res
    | (s - e) == 0    = c : (a ++ b)
    | otherwise = a ++ b
        where
            a = sumEquals1' (s - e) l c
            b = sumEquals1' s l res
            c = res ++ [e]

sumEquals2 :: Int -> [Int] -> Maybe [Int]
sumEquals2 

sumEquals3 :: Int -> [Int] -> [[Int]]
sumEquals3 = sumEquals1
