import Data.Char(digitToInt)

diffSqrs :: Integer -> Integer
diffSqrs n = sum2Nat - sumNat2
    where
        sum2Nat = (n * (n+1) `div` 2) ^ 2
        sumNat2 = n * (n+1) * (2*n + 1) `div` 6

pythagoreanTriplets :: Int -> [(Int, Int, Int)]
pythagoreanTriplets n = filter (\(x, y, z) -> x + y + z == n) triplet
    where triplet = [(a, b, c) | a <- [1..n], b <- [a..n], c <- [b..n], a^2 + b^2 == c^2]

tartaglia :: [[Integer]]
tartaglia = iterate (\x -> (zipWith (+) (0:x) x) ++ [1]) [1]

sumDigits :: Integer -> Integer
sumDigits n = toInteger . sum . map digitToInt $ show n

digitalRoot :: Integer -> Integer
digitalRoot n = head . dropWhile (> 9) $ iterate sumDigits n
