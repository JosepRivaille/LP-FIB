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

-- isomorphic :: Eq a => Tree a -> Tree a -> Bool

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
breadthFirst (Node x Empty Empty) = []
breadthFirst t@(Node x l r) = root t ++ root l ++ root r ++ breadthFirst l ++ breadthFirst r
    where root (Node r _ _) = [r]
