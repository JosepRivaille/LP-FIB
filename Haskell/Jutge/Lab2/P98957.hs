ones :: [Integer]
ones = iterate (+0) 1

nats :: [Integer]
nats = iterate (+1) 0

ints :: [Integer]
ints = [0] ++ concatMap (\x -> [x, -x]) (iterate (+1) 1)

triangulars :: [Integer]
triangulars = scanl (+) 0 (iterate (+1) 1)

factorials :: [Integer]
factorials = scanl (*) 1 (iterate (+1) 1)

fibs :: [Integer]
fibs = [0, 1] ++ fibs' 1 0

fibs' :: Integer -> Integer -> [Integer]
fibs' n1 n2 = [n] ++ fibs' n n1
    where n = n1 + n2

primes :: [Integer]
primes = primes' [] 2

primes' :: [Integer] -> Integer -> [Integer]
primes' l n
    | isPrime l n   = [n] ++ primes' (l ++ [n]) (n+1)
    | otherwise     = primes' l (n+1)

isPrime :: [Integer] -> Integer -> Bool
isPrime l n = and . map (\x -> mod n x > 0) $ takeWhile (\x -> x*x <= n) l

-- hammings :: [Integer]
-- hammings = [1] ++ insertList h5 (insertList h3 h2)
--     where
--         h2 = map (\x -> 2^x) positives
--         h3 = map (\x -> 3^x) positives
--         h5 = map (\x -> 5^x) positives
--         positives = iterate (+1) 1
--
-- insertList :: [Integer] -> [Integer] -> [Integer]
-- insertList l [] = l
-- insertList [] l = l
-- insertList il@(ie:ilt) ol@(oe:olt)
--     | ie > oe   = [oe] ++ insertList il olt
--     | ie < oe   = [ie] ++ insertList ilt ol
--     | otherwise = [oe] ++ insertList ilt olt

-- lookNsay :: [Integer]

tartaglia :: [[Integer]]
tartaglia = iterate (\x -> zipWith (+) (0:x) x ++ [1]) [1]
