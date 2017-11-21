data Queue a = Queue [a] [a] deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue l1 l2) = Queue l1 (x:l2)

pop :: Queue a -> Queue a
pop (Queue [] l) = Queue (drop 1 $ reverse l) []
pop (Queue (e:l1) l2) = Queue l1 l2

top :: Queue a -> a
top (Queue [] l) = last l
top (Queue (e:l1) _) = e

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

instance Eq a => Eq (Queue a) where
    Queue [] [] == Queue [] [] = True
    q1 == q2
        | empty q1 || empty q2 = False
        | otherwise =
            if (top q1) == (top q2)
            then (pop q1) == (pop q2)
            else False
