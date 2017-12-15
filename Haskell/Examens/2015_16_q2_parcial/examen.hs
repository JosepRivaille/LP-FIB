-- Problema 1
allsets :: a -> [[a]]
allsets x = map (\it -> replicate it x) [0..]

-- Problema 2
alldivisors :: Int -> [[Int]]
alldivisors n = map (\d -> take (times d n) $ repeat d) divisors
    where divisors = [d | d <- [2..n], mod n d == 0]

times :: Int -> Int -> Int
times d n = length $ takeWhile (\d -> mod n d == 0) $ iterate (*d) d

-- Problema 3.1
data Expr a = Var String | Const a | Func String [Expr a] deriving (Show)

-- Problema 3.2
constLeafs :: Expr a -> [a]
constLeafs (Const x) = [x]
constLeafs (Func _ exprs) = concatMap constLeafs exprs
constLeafs _ = []

-- Problema 3.3
instance Functor Expr where
    fmap f (Const x)    = Const (f x)
    fmap f (Func x e)   = Func x (map (fmap f) e)
    fmap _ (Var x)      = Var x

-- Problema 4
join :: Eq a => [(String,a)] -> [(String,a)] -> Maybe [(String,a)]
join d1 [] = Just d1
join [] d2 = Just d2
join d1@(h1@(k1,v1):t1) d2@(h2@(k2,v2):t2)
    | k1 < k2   = join t1 d2 >>= (\j -> Just $ [h1] ++ j)
    | k2 < k1   = join d1 t2 >>= (\j -> Just $ [h2] ++ j)
    | otherwise = if v1 == v2
                  then join t1 t2 >>= (\j -> Just $ [h1] ++ j)
                  else Nothing

-- Problema 5
match :: Eq a => Expr a -> Expr a -> Maybe [(String,(Expr a))]
