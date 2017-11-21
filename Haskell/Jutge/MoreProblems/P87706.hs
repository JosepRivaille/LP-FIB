data BST a = E | N a (BST a) (BST a) deriving (Show)

insert :: Ord a => BST a -> a -> BST a
insert E e = (N e E E)
insert bst@(N x l r) e
    | e < x     = (N x (insert l e) r)
    | e > x     = (N x l (insert r e))
    | otherwise = bst

create :: Ord a => [a] -> BST a
create = create' E

create' :: Ord a => BST a -> [a] -> BST a
create' bst [] = bst
create' E (e:l)  = create' (N e E E) l
create' bst (e:l) = create' (insert bst e) l

remove :: Ord a => BST a -> a -> BST a
remove E _ = E
remove bst@(N x l r) e
    | e < x     = (N x (remove l e) r)
    | e > x     = (N x l (remove r e))
    | otherwise = remove' bst

remove' :: Ord a => BST a ->  BST a
remove' (N _ E E) = E
remove' (N _ l E) = l
remove' (N _ E r) = r
remove' (N _ l r) = (N nextInodrer l (remove r . head $ elements r))
    where nextInodrer = head $ elements r

contains :: Ord a => BST a -> a -> Bool
contains E _ = False
contains (N x l r) e
    | e < x     = contains l e
    | e > x     = contains r e
    | otherwise = True

getmax :: BST a -> a
getmax (N x _ E) = x
getmax (N _ _ r) = getmax r

getmin :: BST a -> a
getmin (N x E _) = x
getmin (N _ l _) = getmin l

size :: BST a -> Int
size E = 0
size (N _ l r) = 1 + (size l) + (size r)

elements :: BST a -> [a]
elements E = []
elements (N x l r) = (elements l) ++ [x] ++ (elements r)
