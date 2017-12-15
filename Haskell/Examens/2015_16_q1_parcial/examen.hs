-- Problema 1.1
quadrats :: [Integer]
quadrats = map (^2) [1..]

-- sumQuadrats::Integer -> Bool

-- Problema 2
conway :: [Integer]
conway = map fst $ iterate (\(n, l) -> (next l (last l), l ++ [next l (last l)])) (1, [1, 1])
    where
        fromStart pre = fromIntegral (pre - 1)
        fromEnd l pre = fromIntegral $ fromIntegral l - pre
        next l pre = (l !! (fromStart pre)) + (l !! (fromEnd (length l) pre))

-- Problema 3.1
dc :: (a -> Bool) -> (a -> b) -> (a -> [a]) -> (a -> [b] -> b) -> a -> b
dc tri res par com pro
    | tri pro   = res pro
    | otherwise = com pro $ map (dc tri res par com) $ par pro

-- Problema 3.2
quicksort :: Ord a => [a] -> [a]
quicksort = dc trivial resol parteix combina

trivial :: [a] -> Bool
trivial [] = True
trivial [e] = True
trivial _ = False

resol :: [a] -> [a]
resol [] = []
resol e = e

parteix :: Ord a => [a] -> [[a]]
parteix (x:l) = [left, right]
    where
        left = [e | e <- l, e <= x]
        right = [e | e <- l, e > x]

combina :: [a] -> [[a]] -> [a]
combina (x:_) [left, right] = left ++ [x] ++ right

-- Problema 4
data GTree a = Node a [GTree a] deriving (Show)

-- Problema 4.1
flat :: Eq a => GTree a -> GTree a
flat (Node r l) = (Node r $ concatMap (\c -> flat' r c)  l)

flat' :: Eq a => a -> GTree a -> [GTree a]
flat' pr c@(Node r l) = if pr == r
                        then map flat l
                        else [flat c]

-- Problema 4.2
instance Eq a => Eq (GTree a) where
     (==) t1 t2 = equalTrees (flat t1) (flat t2)

equalTrees :: Eq a => GTree a -> GTree a -> Bool
equalTrees (Node x l) (Node x' l') = x == x' && isPerm l l'

isPerm :: Eq a => [GTree a] -> [GTree a] -> Bool
isPerm l l' = all (\e -> timesIn e l == timesIn e l') l
    where timesIn x = length . filter (== x)
