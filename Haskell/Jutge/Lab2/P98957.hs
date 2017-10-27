ones :: [Integer]
ones = 1:ones
-- ones = iterate (+0) 1

nats :: [Integer]
nats = iterate (+1) 0

ints :: [Integer]
ints = [0] ++ concatMap (\x -> [x, -x]) (iterate (+1) 1)

triangulars :: [Integer]
triangulars = scanl (+) 0 (iterate (+1) 1)

factorials :: [Integer]
factorials = scanl (*) 1 (iterate (+1) 1)

fibs :: [Integer]
fibs = [0, 1] ++ zipWith (+) fibs (tail fibs)
-- fibs = [0, 1] ++ fibs' 1 0

-- fibs' :: Integer -> Integer -> [Integer]
-- fibs' n1 n2 = [n] ++ fibs' n n1
--     where n = n1 + n2

primes :: [Integer]
primes = sieve (iterate (+1) 2)
--primes = primes' [] 2

sieve :: [Integer] -> [Integer]
sieve (e:l) = [e] ++ sieve (filter (\x -> mod x e /= 0) l)

-- primes' :: [Integer] -> Integer -> [Integer]
-- primes' l n
--     | isPrime l n   = [n] ++ primes' (l ++ [n]) (n+1)
--     | otherwise     = primes' l (n+1)

-- isPrime :: [Integer] -> Integer -> Bool
-- isPrime l n = and . map (\x -> mod n x > 0) $ takeWhile (\x -> x*x <= n) l

hammings :: [Integer]
hammings = [1] ++ (map (*2) hammings) `merge` (map (*3) hammings) `merge` (map (*5) hammings)

merge :: [Integer] -> [Integer] -> [Integer]
merge l1@(el1:tl1) l2@(el2:tl2)
    | el1 < el2 = [el1] ++ merge tl1 l2
    | el1 > el2 = [el2] ++ merge l1 tl2
    | otherwise = [el1] ++ merge tl1 tl2

lookNsay :: [Integer]
lookNsay = [1] ++ iterate (\x -> read $ lookNsay' (show x) 1) 11

lookNsay' :: String -> Integer -> String
lookNsay' [e] count = show count ++ [e]
lookNsay' (e1:ts@(e2:s)) count
    | e1 == e2  = lookNsay' ts (count+1)
    | otherwise = show count ++ [e1] ++ lookNsay' ts 1

tartaglia :: [[Integer]]
tartaglia = iterate (\x -> zipWith (+) (0:x) x ++ [1]) [1]
