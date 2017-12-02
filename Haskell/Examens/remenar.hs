shuffleOnce :: [a] -> [a]
shuffleOnce l = flatten (zipWith (\x y -> [y] ++ [x]) left right) ++ extra
    where
        (left, right) = splitAt (div (length l) 2) l
        extra = if mod (length l) 2 == 0 then [] else [last right]

flatten :: [[a]] -> [a]
flatten [] = []
flatten (e:l) = e ++ (flatten l)

shuffleBack :: Eq a => [a] -> Int
shuffleBack l = 1 + (length $ takeWhile (\e -> e /= l) $ tail $ iterate (\e -> shuffleOnce e) l)

segments :: Ord a => [a] -> [[a]]
segments l = foldr addOrConcat []

addOrConcat :: Ord a => a => [[a]]
addOrConcat e [] = [[e]]
addOrConcat e l@(a@(ha:ta):b)
    | e <= z    = [he] ++ l
    | otherwise = e

mergeSegments :: Ord a => [[a]] -> [[a]]
mergeSegments l = zip ()

merge :: [a] -> [a] -> [[a]]
merge l1 l2 = concatMap (\(x,y) -> [x,y]) (zip l1 l2)

-- Exercici 3

data FExpr a = Const a | Func FExpr String [FExpr a] deriving (Show)

flatten :: FExpr a -> FExpr a
flatten (Const x) = Const x
flatten (Func s args) = Func s $ concatMap (transform . flatten) args'
    where
        transform (Const x) = [Const x]
        transform expr@(Func s' args')
            | s == s'   = args'
            | otherwise = [expr]
