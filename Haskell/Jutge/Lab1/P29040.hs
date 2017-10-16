qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:l) = (qsort left) ++ [x] ++ (qsort right)
    where
        left = [e | e <- l, e <= x]
        right = [e | e <- l, e > x]