sumMultiples35 :: Integer -> Integer
sumMultiples35 n = sumMultiples n 3 + sumMultiples n 5 - sumMultiples n 15

sumMultiples :: Integer -> Integer -> Integer
sumMultiples n x = x * z * (z + 1) `div` 2
    where z = div (n - 1) x

fibonacci :: Int -> Integer
fibonacci n = last $ take (n + 1) fibs

sumEvenFibonaccis :: Integer -> Integer
sumEvenFibonaccis n = foldl1 (+) $ filter even $ takeWhile (<n) fibs

fibs :: [Integer]
fibs = [0, 1] ++ zipWith (+) fibs (tail fibs)

largestPrimeFactor :: Int -> Int
largestPrimeFactor 0 = 0
largestPrimeFactor 1 = 1
largestPrimeFactor n = last $ sieve [x | x <- [2..n], mod n x == 0]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (e:l) = [e] ++ sieve (filter (\x -> mod x e /= 0) l)

isPalindromic :: Integer -> Bool
isPalindromic n = sn == (reverse sn)
    where sn = show n
