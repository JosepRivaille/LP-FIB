primes :: [Int]
primes = 2 : 3 : sieve (tail primes) [5,7..]

sieve :: [Int] -> [Int] -> [Int]
sieve (p:ps) xs = h ++ sieve ps [ x | x <- t, (rem x p)/=0 ]
  where (h,~(_:t)) = span (<p*p) xs



-- factorization
fac :: Int -> [Int] -> [Int]
fac n pl@(p:px)
  | n==1 = []
  | p==n = [n]
  | (mod n p)==0 = p : fac (div n p) pl
  | otherwise = fac n px


factorial :: Int -> [Int]
factorial n = fac n primes

countDivisors :: [Int] -> Int
countDivisors [] = 1
countDivisors l@(p:px) = (countDivisors rp) * ((length lp)+1)
    where (lp,rp) = span (==p) l

analyze :: Int -> Either Int Bool
analyze 1 = Right False
analyze n
  | cd<=12 = Left cd 
  | otherwise = Left cd
  where
      cd = (countDivisors fl)-1
      fl = factorial n
