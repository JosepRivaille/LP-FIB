eql :: [Int] -> [Int] -> Bool
eql l1 l2 =  sameLength && (and (zipWith (==) l1 l2))
    where sameLength = (length l1) == (length l2)

prod :: [Int] -> Int
prod l = foldl (*) 1 l

prodOfEvens :: [Int] -> Int
prodOfEvens = prod . filter even

powersOf2 :: [Int]
powersOf2 = zipWith (^) [2, 2..] [0..]

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct l1 l2 = sum (zipWith (*) l1 l2)
