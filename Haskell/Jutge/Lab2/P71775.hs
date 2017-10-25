countIf :: (Int -> Bool) -> [Int] -> Int
countIf f = foldl (\acc e -> acc + (evaluate f e)) 0
    where
        evaluate :: (Int -> Bool) -> Int -> Int
        evaluate f e = if f e then 1 else 0

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam l = foldl (\acc f -> acc ++ [map f l]) []
-- pam _ [] = []
-- pam l (f:lf) = [map f l]  ++ (pam l lf)

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 l lf = foldl (\acc e -> acc ++ [map (\f -> f e) lf]) [] l
-- pam2 [] _ = []
-- pam2 (e:l) lf = [map (\f -> f e) lf] ++ (pam2 l lf)

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl ffil ffol b l = foldl ffol b (filter ffil l)

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert f l x = takeWhile (\e -> f e x) l ++ [x] ++ dropWhile (\e -> f e x) l
-- insert _ [] x = [x]
-- insert f cl@(e:l) x
--    | f e x     = [e] ++ (insert f l x)
--    | otherwise = [x] ++ cl

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort f = foldl (\acc e -> insert f acc e) []
