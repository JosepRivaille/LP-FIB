select :: Ord a => [a] -> Int -> a
select (x:xs) k
    | (k-1) < l     = select left k
    | (k-1) > l     = select right (k-l-1)
    | otherwise = x
    where
        left = [e | e <- xs, e < x]
        right = [e | e <- xs, e >= x]
        l = length left
