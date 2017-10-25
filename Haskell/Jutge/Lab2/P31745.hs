flatten :: [[Int]] -> [Int]
flatten = foldl (++) []

myLength :: String -> Int
myLength = foldr (\_ y -> y + 1) 0

myReverse :: [Int] -> [Int]
myReverse = foldr (\x y -> y ++ [x]) []

countIn :: [[Int]] -> Int -> [Int]
countIn l x = map (\sl -> length (filter (== x) sl)) l

firstWord :: String -> String
firstWord = takeWhile (/= ' ') . dropWhile (== ' ')
