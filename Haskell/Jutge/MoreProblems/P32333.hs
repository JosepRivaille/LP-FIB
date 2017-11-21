fact1 :: Integer -> Integer -- Recursion
fact1 0 = 1
fact1 n = fact1 (n - 1) * n

fact2 :: Integer -> Integer -- End Recursion
fact2 0 = 1
fact2 n = n * fact2 (n - 1)

fact3 :: Integer -> Integer -- No recursion
fact3 n = product [1..n]

fact4 :: Integer -> Integer -- If - then - else
fact4 n = if n == 0 then 1 else n * fact4 (n - 1)

fact5 :: Integer -> Integer -- Guards
fact5 n
    | n == 0    = 1
    | otherwise = n * fact5 (n - 1)

fact6 :: Integer -> Integer -- Foldl
fact6 n = foldl (*) 1 [1..n]

fact7 :: Integer -> Integer -- Map
fact7 n = last $ map (\x -> foldl (*) 1 [1..x]) [0..n]

fact8 :: Integer -> Integer -- Infinite lists
fact8 n = snd . last . take n' $ iterate (\(x, fx) -> (x + 1, x * fx)) (1, 1)
    where n' = fromIntegral $ n + 1
