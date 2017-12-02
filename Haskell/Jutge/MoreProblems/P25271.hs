import Data.List

permutations1 :: [a] -> [[a]]
permutations1 [] = [[]]
permutations1 l = [hl:ptl | (hl, tl) <- select l, ptl <- permutations1 tl]
    where
        select :: [a] -> [(a, [a])]
        select [] = []
        select (hx:tx) = (hx, tx) : [(hy, hx:ty) | (hy, ty) <- select tx]

permutations2 :: Int -> [[Int]]
permutations2 n = permutations1 [1..n]

permutations3 :: [a] -> [[a]]


permutations4 :: Int -> [[Int]]
permutations4 n = permutations3 [1..n]
