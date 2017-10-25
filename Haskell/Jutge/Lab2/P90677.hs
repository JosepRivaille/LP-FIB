myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f b [] = b
myFoldl f b (e:l) = myFoldl f (f b e) l

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b [] = b
myFoldr f b (e:l) = f e (myFoldr f b l)

myIterate :: (a -> a) -> a -> [a]
myIterate f b = [b] ++ myIterate f (f b)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil c f b
    | c b       = b
    | otherwise = myUntil c f (f b)

myMap :: (a -> b) -> [a] -> [b]
myMap f l = foldl (\acc e -> acc ++ [f e]) [] l

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = concatMap (\x -> if f x then [x] else []) l

myAll :: (a -> Bool) -> [a] -> Bool
myAll f l = and (map f l)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f l = or (map f l)

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (el1:l1) (el2:l2) = [(el1, el2)] ++ myZip l1 l2

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f l1 l2 = map (\x -> f (fst x) (snd x)) $ zip l1 l2
