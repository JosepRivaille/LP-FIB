-- Problema 1.1
shuffleOnce :: [a] -> [a]
shuffleOnce l = (concatMap (\(el, er) -> [el, er]) $ zip right left) ++ extra
    where
        len = div (length l) 2
        left = take len l ; right = drop len l
        extra = if mod (length l) 2 == 0 then [] else [last l]

-- Problema 1.2
shuffleBack :: Eq a => [a] -> Int
shuffleBack l = 1 + (length $ takeWhile (\l' -> l /= l') $ shuffleList)
    where shuffleList = iterate shuffleOnce (shuffleOnce l)

-- Problema 2.1
segments :: Ord a => [a] -> [[a]]
segments = foldr addOrConcat []

addOrConcat :: Ord a => a -> [[a]] -> [[a]]
addOrConcat e [] = [[e]]
addOrConcat e l@(c@(ch:_):t)
    | e < ch    = (e:c):t
    | otherwise = [e]:l

-- Problema 2.2
mergeSegments :: Ord a => [[a]] -> [[a]]
mergeSegments [] = []
mergeSegments (l1:[]) = [l1]
mergeSegments (l1:l2:l) = [merge l1 l2] ++ mergeSegments l

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] l = l
merge l [] = l
merge l1@(h1:t1) l2@(h2:t2)
    | h1 < h2   = [h1] ++ merge t1 l2
    | h2 < h1   = [h2] ++ merge l1 t2
    | otherwise = [h1, h2] ++ merge t1 t2

-- Problema 2.3
mergeSegmentsort :: Ord a => [a] -> [a]
mergeSegmentsort l = head $ head $ dropWhile (\it -> length it > 1) $ iterate mergeSegments (segments l)

-- Problema 3.1
data FExpr a = Const a | Func String [FExpr a] deriving (Show)

-- Problema 3.2
flatten :: FExpr a -> FExpr a
flatten (Const c) = Const c
flatten (Func x l) = Func x $ concatMap (\child -> flatten' x $ flatten child) l

flatten' :: String -> FExpr a -> [FExpr a]
flatten' _ (Const c) = [Const c]
flatten' x' f@(Func x l)
    | x == x'   = l
    | otherwise = [f]

-- Problema 3.3
instance Eq a => Eq (FExpr a) where
    (==) fe1 fe2 = equalExpr (flatten fe1) (flatten fe2)

equalExpr :: Eq a => FExpr a -> FExpr a -> Bool
equalExpr (Const c) (Const c') = c == c'
equalExpr (Func x l) (Func x' l') = x == x' && isPerm l l'
equalExpr _ _ = False

isPerm :: Eq a => [FExpr a] -> [FExpr a] -> Bool
isPerm l l' = all (\e -> timesIn e l == timesIn e l') l
    where timesIn x = length . filter (== x)
