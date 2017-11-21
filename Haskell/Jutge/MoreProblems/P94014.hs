fib :: Int -> Integer
fib n = fst $ fdFib n

{-
    Fast Doubling Fibonacci method:
    FFD(k) = FFD(k/2) * (2*FFD(k/2 - 1) - FFD(k/2))
    FFD(k+1) = FFD(k/2 + 1)^2 + FFD(k/2)^2
-}
fdFib :: Int -> (Integer, Integer)
fdFib 0 = (0, 1)
fdFib n
    | mod n 2 == 0  = (c, d)
    | otherwise     = (d, c + d)
    where
        (a, b) = fdFib (div n 2)
        c = a * (b * 2 - a)
        d = a * a + b * b

-- Karatsuba's fast multiplication of two given integers
karatsuba :: Integer -> Integer -> Integer
karatsuba n m
    | n <= 100 || m <= 100  = n * m
    | otherwise             = z2*10 ^ (2 * half) + (z1-z2-z0) * 10^half + z0
    where
        z0 = karatsuba lowN lowM
        z1 = karatsuba (lowN + highN) (lowM + highM)
        z2 = karatsuba highN highM
        digits = max (numDigits n) (numDigits m)
        half = div digits 2
        (highN, lowN) = splitDigits n half
        (highM, lowM) = splitDigits m half

-- Counts number of digits of n
numDigits :: Integer -> Integer
numDigits n
    | n == 0    = 1
    | n == 1000 = 4
    | otherwise = floor $ logBase 10 (fromInteger n) + 1

-- Given a number xy, splits into two numbers at position n
splitDigits :: Integer -> Integer -> (Integer, Integer)
splitDigits xy n = (x, y)
    where
        x = div xy (10^n)
        y = xy - x * 10^n
