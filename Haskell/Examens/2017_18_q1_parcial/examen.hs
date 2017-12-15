-- -------------------------------------------------
-- Problema 1
-- -------------------------------------------------

eval1 :: String -> Int
eval1 l = head $ eval1' $ words l

eval1' :: [String] -> [Int]
eval1' [e] = [read e]
eval1' l = eval1' $ result ++ l'
    where
        result = map show (eval current op)
        current = map read $ takeWhile (not.isOp) l
        (op:l') = drop (length current) l

-- ComĂş problema 1 i 2
eval :: [Int] -> String -> [Int]
eval stack op
    | op == "+" = stack' ++ [l2 + l1]
    | op == "-" = stack' ++ [l2 - l1]
    | op == "*" = stack' ++ [l2 * l1]
    | op == "/" = stack' ++ [div l2 l1]
    where
        (l1:l2:l) = reverse stack
        stack' = reverse l

-- ComĂş problema 1 i 2
isOp :: String -> Bool
isOp "+" = True
isOp "-" = True
isOp "*" = True
isOp "/" = True
isOp _ = False

-- -------------------------------------------------
-- Problema 2
-- -------------------------------------------------

eval2 :: String -> Int
eval2 l = read $ eval2' $ words l

eval2' :: [String] -> String
eval2' l = head.head $ dropWhile (\it -> length it > 1) iterations
    where iterations = iterate (\it' -> let result = map show (eval current op)
                                            current = map read $ takeWhile (not.isOp) it'
                                            (op:l') = drop (length current) it'
                                        in result ++ l') l

-- -------------------------------------------------
-- Problema 3
-- -------------------------------------------------

fsmap :: a -> [a -> a] -> a
fsmap x fs = foldl (\acc f -> f acc) x fs

-- -------------------------------------------------
-- Problema 4
-- -------------------------------------------------

divideNconquer :: (a -> Maybe b) -> (a -> (a, a)) -> (a -> (a, a) -> (b, b) -> b) -> a -> b
divideNconquer base divide conquer x
    | isTrivial trivial  = (\(Just x) -> x) trivial
    | otherwise          = let (left, right) = divide x
                               left' = divideNconquer base divide conquer left
                               right' = divideNconquer base divide conquer right
                           in conquer x (left, right) (left', right')
    where
        trivial = base x
        isTrivial :: Maybe a -> Bool
        isTrivial Nothing = False
        isTrivial _ = True


quickSort :: [Int] -> [Int]
quickSort l =  divideNconquer base divide conquer l

base :: [a] -> Maybe [a]
base [] = Just []
base [e] = Just [e]
base _ = Nothing

divide :: Ord a => [a] -> ([a], [a])
divide (x:l) = (left, right)
    where
        left = [e | e <- l, e <= x]
        right = [e | e <- l, e > x]

conquer :: [a] -> ([a], [a]) -> ([a], [a]) -> [a]
conquer p _ ss = (fst ss) ++ [head p] ++ (snd ss)

-- -------------------------------------------------
-- Problema 5
-- -------------------------------------------------

data Racional = Racional Integer Integer

racional :: Integer -> Integer -> Racional
racional n d = Racional n d

numerador :: Racional -> Integer
numerador (Racional n d) = div n (gcd n d)

denominador :: Racional -> Integer
denominador (Racional n d) = div d (gcd n d)

instance Show Racional where
    show r = (show $ numerador r) ++ "/" ++ (show $ denominador r)

instance Eq Racional where
    (==) r1 r2 = numerador r1 == numerador r2 && (denominador r1 == denominador r2)

-- -------------------------------------------------
-- Problema 6: NO CONTESTAT
-- -------------------------------------------------

racionals :: [Racional]
racionals = [initial] ++ (racionals' (racional 1 1))
    where initial = racional 1 1

racionals' :: Racional -> [Racional]
racionals' r = [left, right] ++ (racionals' left) ++ (racionals' right)
    where
        a = numerador r ; b = denominador r
        left = racional a (a + b)
        right = racional (a + b) b
