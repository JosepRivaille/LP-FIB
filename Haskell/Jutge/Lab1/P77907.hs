absValue :: Int -> Int
absValue x
    | x >= 0        = x
    | otherwise     = -x

power :: Int -> Int -> Int
power x p = x ^ p

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = length [x | x <- [2..(n-1)], mod n x == 0] == 0

{-
isPrime 0 = False
isPrime 1 = False
isprime n = isPrime' 2
    where
        isPrime' :: Int -> Bool
        isPrime' c
            | c*c > n       = True
            | mod n c == 0  = False
            | otherwise     = isPrime' (c+1)
-}

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n-1) + slowFib (n-2)

quickFib' :: Int -> (Int, Int)
quickFib' 0 = (0, 0)
quickFib' 1 = (0, 1)
quickFib' n = (fn1, fn1 + fn2)
    where
        (fn2, fn1) = quickFib' (n-1)

quickFib :: Int -> Int
quickFib n = snd (quickFib' n)