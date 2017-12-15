-- Problema 1.1
myconcat :: [[a]] -> [a]
myconcat l = [y | x <- l, y <- x]

-- Problema 1.2
concat3 :: [[[a]]] -> [a]
concat3 l = [z | x <- l, y <- x, z <- y]

-- Problema 2
fold2r :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
fold2r f ini l1 l2 = foldr (\(x, y) acc -> f x y acc) ini $ zip l1 l2

-- Problema 3
mix :: [a] -> [a] -> [a]
mix l1 l2 = (concatMap (\(x, y) -> [x, y]) $ zip l1 l2) ++ extra
    where
        len1 = length l1 ; len2 = length l2
        extra = if len1 > len2 then drop len2 l1 else drop len1 l2

lmix :: [Int] -> [a] -> [a]
lmix is l = foldl (\acc i -> mix (take i acc) (drop i acc)) l is

-- Problema 4
dPascal :: Int -> [Integer]
dPascal k = map (\n -> comb n k) [k..]

comb :: Int -> Int -> Integer
comb n k = div (fact !! n) ((fact !! k) * (fact !! (n - k)))

fact :: [Integer]
fact = scanl (*) 1 [1..]

-- Problema 5
data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving (Show)

buildTreeF :: [[a]] -> BTree a
buildTreeF lev = head $ buildTreeF' lev

buildTreeF' :: [[a]] -> [BTree a]
buildTreeF' [] = [Empty]
buildTreeF' [l] = map (\x -> Node x Empty Empty) l
buildTreeF' (x:l) = buildTreeF'' x (buildTreeF' l)

buildTreeF'' :: [a] -> [BTree a] -> [BTree a]
buildTreeF'' [] _ = [Empty]
buildTreeF'' (x:l) ch = [Node x left right] ++ buildTreeF'' l l'
    where
        (h, l') = splitAt 2 ch
        left = head h ; right = head $ tail h

-- Problema 6.1
class Lit a where
    unary :: a -> a
    binary :: a -> a -> a
    list :: [a] -> a

-- Problema 6.2
data Expr a = Val a
            | Unary (Expr a)
            | Binary (Expr a) (Expr a)
            | List [Expr a]
            deriving (Show)

-- Problema 6.3
eval :: Lit a => Expr a -> a
eval (Val v) = v
eval (Unary x) = unary (eval x)
eval (Binary x y) = binary (eval x) (eval y)
eval (List l) = list $ map (eval) l

-- Problema 6.4
instance Lit Int where
    unary x = -x
    binary x y = x + y
    list = sum
