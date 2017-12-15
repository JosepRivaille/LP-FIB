-- Problema 1
mergelist :: Ord a => [[a]] -> [a]
mergelist = foldl merge []

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] l = l
merge l [] = l
merge l1@(h1:t1) l2@(h2:t2)
    | h1 < h2   = [h1] ++ merge t1 l2
    | h2 < h1   = [h2] ++ merge l1 t2
    | otherwise = [h1] ++ merge t1 t2

-- Problema 2
mults :: [Integer] -> [Integer]
mults l = mergelist $ map (\x -> 1:[e | e <- [1, x..], mod e x == 0]) l

-- Problema 3.1
data Procs a = End |
               Skip (Procs a) |
               Unary (a -> a) (Procs a) |
               Binary (a -> a -> a) (Procs a)

-- Problema 3.2
exec :: [a] -> (Procs a) -> [a]
exec [] _ = []
exec l End = l
exec (e:l) (Skip procs) = [e] ++ exec l procs
exec (e:l) (Unary f procs) = exec ((f e):l) procs
exec [e] (Binary f procs) = exec [f e e] procs
exec (e1:e2:l) (Binary f procs) = exec ((f e1 e2):l) procs

-- Problema 4.1
class Container c where
    emptyC :: c a -> Bool
    lengthC :: c a -> Int
    firstC :: c a -> a
    popC :: c a -> c a

-- Problema 4.2
instance Container [] where
    emptyC []   = True
    emptyC _    = False
    lengthC     = length
    firstC      = head
    popC        = tail

-- Problema 4.3
data Tree a = Empty | Node a [Tree a] deriving (Show)

-- Problema 4.4
instance Container Tree where
    emptyC Empty        = True
    emptyC _            = False
    lengthC Empty       = 0
    lengthC (Node _ l)  = 1 + (sum $ map lengthC l)
    firstC (Node x _)   = x
    popC (Node _ [])    = Empty
    popC (Node x (n:l)) = if emptyC n
                          then popC (Node x l)
                          else (Node (firstC n) l)

-- Problema 4.5
iterator :: Container c => c a -> [a]
iterator c
    | emptyC c = []
    | otherwise = map firstC $ takeWhile (not . emptyC) $ iterate popC c
