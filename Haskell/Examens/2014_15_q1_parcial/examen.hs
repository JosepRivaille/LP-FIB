-- Problema 1
prefsufs :: [a] -> [[a]]
prefsufs l = prefs ++ suffs
    where
        len = length l
        prefs = map (\i -> take i l) [1..len]
        suffs = map (\i -> drop i l) [1..(len-1)]

-- Problema 2.1
fixedPoint :: (Eq a, Num a) => (a -> a) -> a -> a
fixedPoint f x = head $ dropWhile (\i -> (f i) /= i) $ iterate f x

-- Problema 2.2
newtonSqrt :: (Eq a, Num a, Fractional a) => a -> a
newtonSqrt y = fixedPoint nsf 1.0
    where nsf x = (y / x + x) / 2

-- Problema 3.1
data Polynomial a = P [a] deriving (Show)

instance (Eq a, Num a) => Eq (Polynomial a) where
    (==) (P x) (P y)    = all (\(c1, c2) -> c1 == c2) (zip x y)

-- Problema 3.2
instance Num a => Num (Polynomial a) where
    (P x) + (P y)       = (P $ zipWith (+) x y)
    (P x) - (P y)       = (P $ zipWith (-) x y)
    (P x) * (P y)       = let psx = prefsufs x
                              psy = map reverse $ prefsufs y
                          in (P $ zipWith (\xi yi -> sum $ zipWith (*) xi yi) psx psy)
    abs (P x)           = (P $ map abs x)
    signum (P x)        = (P $ [signum $ head x] ++ (tail x))
    fromInteger i       = (P [fromInteger i])

-- Problema 4.1
data AndOr a = ALeaf a | Nand [OrAnd a]
data OrAnd a = OLeaf a | Nor [AndOr a]

-- Problema 4.2
eval :: (a -> Bool) -> (AndOr a) -> Bool
eval f (ALeaf e) = f e
eval f (Nand l) = and $ map (eval' f) l

eval' :: (a -> Bool) -> (OrAnd a) -> Bool
eval' f (OLeaf e) = f e
eval' f (Nor l) = or $ map (eval f) l
