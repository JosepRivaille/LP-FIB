qsort :: Bool -> [Int] -> [Int]
qsort _ [] = []
qsort asc (e:l) = (qsort asc left) ++ [e] ++ (qsort asc right)
    where
        left = [x | x <- l, if asc then x <= e else x > e]
        right = [x | x <- l, if asc then x > e else x <= e]

minProd :: [Int] -> [Int] -> Int
minProd l1 l2 = sum $ zipWith (*) (qsort True l1) (qsort False l2)
