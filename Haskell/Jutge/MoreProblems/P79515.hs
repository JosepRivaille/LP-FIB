data AVL a = E | N a Int (AVL a) (AVL a) deriving (Show)

insert :: Ord a => AVL a -> a > AVL a
insert E e = (N e E E)
insert avl@(N x l r) e
    | e < x     = balance (N x (insert l e) r)
    | e > x     = balance (N x l (insert r e))
    | otherwise = avl

balance :: Ord a => AVL a -> AVL a


llRot ::

lrRot ::

rlRot ::

rrRot ::

create :: Ord a => [a] -> AVL a
create = create' E

create' :: Ord a => AVL a -> [a] -> AVL a
create' avl [] = avl
create' E (e:l) = create' (N e 1 E E) l
create' avl (e:l) = create' (insert avl e) l

check :: AVL a -> (Bool, Int)
check (N _ _ (N _ h1 _ _) (N _ h2 _ _))
    | h1 == h2  = check
    | otherwise = (False, -99)
check _ = (False, -99)
    where merge
