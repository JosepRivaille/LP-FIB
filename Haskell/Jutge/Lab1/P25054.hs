myLength :: [Int] -> Int
myLength [] = 0
myLength (_:l) = myLength l + 1

myMaximum :: [Int] -> Int
myMaximum [e] = e
myMaximum (e:l)
    | e > m         = e
    | otherwise     = m
        where
            m = myMaximum(l)    

average :: [Int] -> Float
average l = (fromIntegral (mySum l)) / (fromIntegral (myLength l))
    where
        mySum :: [Int] -> Int
        mySum [] = 0
        mySum (e:l) = mySum(l) + e

buildPalindrome :: [Int] -> [Int]
buildPalindrome l = myReverse l ++ l
    where
        myReverse :: [Int] -> [Int]
        myReverse [] = []
        myReverse (e:l) = myReverse l ++ [e]

remove :: [Int] -> [Int] -> [Int]
remove [] _ = []
remove l [] = l
remove l (e:r) = remove (removeElement l e) r
    where
        removeElement :: [Int] -> Int -> [Int]
        removeElement [] e = []
        removeElement (e:l) r
            | r == e    = removeElement l r
            | otherwise = [e] ++ removeElement l r

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (e:l) = e ++ (flatten l)

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([], [])
oddsNevens (e:l)
    | mod e 2 == 0  = (odds, [e] ++ evens)
    | otherwise     = ([e] ++ odds, evens)
        where (odds, evens) = oddsNevens(l)

primeDivisors :: Int -> [Int]
primeDivisors n = [e | e <- [0..n], (isPrime e) && (mod n e == 0)]
    where
        isPrime 0 = False
        isPrime 1 = False
        isPrime n = not (hasDivisors 2)
            where
                hasDivisors :: Int -> Bool
                hasDivisors c
                    | c*c > n       = False
                    | mod n c == 0  = True
                    | otherwise     = hasDivisors (c + 1)