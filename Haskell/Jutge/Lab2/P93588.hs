myMap :: (a -> b) -> [a] -> [b]
myMap f l = [f e | e <- l]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = [e | e <- l, f e]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f l1 l2 = [f el1 el2 | (el1, el2) <- zip l1 l2]

thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify l1 l2 = [(el1, el2) | el1 <- l1, el2 <- l2, mod el1 el2 == 0]

factors :: Int -> [Int]
factors n = [f | f <- [1..n], mod n f == 0]
