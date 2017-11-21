data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ l r) = 1 + size l + size r

height :: Tree a -> Int
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal (Node x1 l1 r1) (Node x2 l2 r2) = (x1 == x2) && (equal l1 l2) && (equal r1 r2)
equal _ _ = False

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic (Node x1 l1 r1) (Node x2 l2 r2)
    | x1 == x2  = firstMutation || secondMutation
    | otherwise = False
    where
        firstMutation = isomorphic l1 l2 && isomorphic r1 r2
        secondMutation = isomorphic l1 r2 && isomorphic r1 l2
isomorphic _ _ = False

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x l r) = [x] ++ (preOrder l) ++ (preOrder r)

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node x l r) = (postOrder l) ++ (postOrder r) ++ [x]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x l r) = (inOrder l) ++ [x] ++ (inOrder r)

breadthFirst :: Tree a -> [a]
breadthFirst Empty = []
breadthFirst t = breadthFirst' [t]

breadthFirst' :: [Tree a] -> [a]
breadthFirst' [] = []
breadthFirst' (Empty:tt) = breadthFirst' tt
breadthFirst' ((Node x l r):tt) = [x] ++ (breadthFirst' $ tt ++ [l, r])

build :: Eq a => [a] -> [a] -> Tree a
build _ [] = Empty
build (x:pre) ino = Node x (build preLeft inoLeft) (build preRight inoRight)
    where
        preLeft = intersect pre inoLeft
        preRight = intersect pre inoRight
        splitIno = splitVal x ino
        inoLeft = fst splitIno
        inoRight = snd splitIno

splitVal :: Eq a => a -> [a] -> ([a], [a])
splitVal _ [] = ([], [])
splitVal e l = (leftPart, rightPart)
    where
        leftPart = takeWhile (\x -> x /= e) l
        tailList = dropWhile (\x -> x /= e) l
        rightPart = if tailList == [] then [] else tail tailList

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect l1 l2 = [e | e <- l1, any (== e) l2]

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty Empty = Empty
overlap _ Empty x = x
overlap _ x Empty = x
overlap f (Node x1 l1 r1) (Node x2 l2 r2) = applyF
    where applyF = Node (f x1 x2) (overlap f l1 l2) (overlap f r1 r2)
