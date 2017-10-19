insert :: [Int] -> Int -> [Int]
insert [] e = [e]
insert l x = left ++ [x] ++ right
    where
        left = [e | e <- l, e <= x]
        right = [e | e <- l, e > x]

isort :: [Int] -> [Int]
isort [] = []
isort (e:l) = insert (isort l) e

remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (e:l) x
    | e == x    = l
    | otherwise = [e] ++ (remove l x)

ssort :: [Int] -> [Int]
ssort [] = []
ssort l = [m] ++ ssort (remove l m)
    where
        m = minimum(l)

firstHalf :: [a] -> [a]
firstHalf l = take (div (length l) 2) l

secondHalf :: [a] -> [a]
secondHalf l = drop (div (length l) 2) l

merge :: [Int] -> [Int] -> [Int]
merge [] l = l
merge l [] = l
merge a@(e1:l1) b@(e2:l2)
    | e1 <= e2  = [e1] ++ (merge l1 b)
    | otherwise = [e2] ++ (merge a l2)

msort :: [Int] -> [Int]
msort [] = []
msort [e] = [e]
msort l = merge (msort $ firstHalf l) (msort $ secondHalf l)

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:l) = (qsort left) ++ [x] ++ (qsort right)
    where
        left = [e | e <- l, e <= x]
        right = [e | e <- l, e > x]

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (x:l) = (genQsort left) ++ [x] ++ (genQsort right)
    where
        left = [e | e <- l, e <= x]
        right = [e | e <- l, e > x]
