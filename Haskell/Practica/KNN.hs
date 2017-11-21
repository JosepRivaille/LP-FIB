import System.IO

import Numeric

import Data.List.Split
import Data.List
import Data.Ord

data Flower = Flower {
    clas :: String,
    sepL :: Float,
    sepW :: Float,
    petL :: Float,
    petW :: Float
} deriving (Show)

-- Euclidean distance given two flowers
euclideanDistance :: Flower -> Flower -> Float
euclideanDistance f1 f2 = sqrt $ sdSepL + sdSepW + sdPetL + sdPetW
    where
        sdSepL = (sepL f1 - sepL f2) ** 2
        sdSepW = (sepW f1 - sepW f2) ** 2
        sdPetL = (petL f1 - petL f2) ** 2
        sdPetW = (petW f1 - petW f2) ** 2

-- Manhattan distance given two flowers
manhattanDistance :: Flower -> Flower -> Float
manhattanDistance f1 f2 = adSepL + adSepW + adPetL + adPetW
    where
        adSepL = abs $ (sepL f1) - (sepL f2)
        adSepW = abs $ (sepW f1) - (sepW f2)
        adPetL = abs $ (petL f1) - (petL f2)
        adPetW = abs $ (petW f1) - (petW f2)

-- Evaluates a distancie function given a flower and a list of flowers
calcDistsClass :: Flower -> (Flower -> Flower -> Float) -> [Flower] -> [(String, Float)]
calcDistsClass f df = map (\x -> (clas x, df f x))

-- Reduced list with sum of same-class scoress and number of appearances
countAppearances :: [(String, Float)] -> ((String, Float), Int) -> [((String, Float), Int)]
countAppearances [(e, ed)] current@((clas, clasd), count)
    | e == clas = [((clas, clasd + ed), count + 1)]
    | otherwise = [current, ((e, ed), 1)]
countAppearances ((e, ed):l) current@((clas, clasd), count)
    | e == clas = countAppearances l ((clas, clasd + ed), count + 1)
    | otherwise = [current] ++ countAppearances l ((e, ed), 1)

-- Reduced list with sum of same-class scores
sumWeights :: [(String, Float)] -> (String, Float) -> [(String, Float)]
sumWeights [(c, w)] current@(clas, weight)
    | c == clas = [(clas, weight + w)]
    | otherwise = [current, (c, w)]
sumWeights ((c, w):l) current@(clas, weight)
    | c == clas = sumWeights l (clas, weight + w)
    | otherwise = [current] ++ sumWeights l (c, w)

-- Class corresponding to max appearing class
maxAppearances :: [(String, Float)] -> String
maxAppearances l = fst $ fst $ head $ sortBy countDist (countAppearances sl (head sl, 0))
    where
        sl = sortBy (\(k1, _) (k2, _) -> compare k1 k2) l
        countDist :: ((String, Float), Int) -> ((String, Float), Int) -> Ordering
        countDist ((_, d1), c1) ((_, d2), c2)
            | c1 < c2   = GT
            | c2 < c1   = LT
            | otherwise = compare d1 d2

-- Class corresponding to greater weighted score
maxWeights :: [(String, Float)] -> String
maxWeights l = fst $ head $ sortBy (flip $ comparing snd) (sumWeights sl (head sl))
    where sl = sortBy (comparing fst) l

-- Simple vote for kth greater scores
simpleVote :: Int -> [(String, Float)] -> String
simpleVote k dc = maxAppearances $ vote
    where vote = take k $ sortBy (comparing snd) dc

-- Weighted vote for kth greater scores
weightedVote :: Int -> [(String, Float)] -> String
weightedVote k dc = maxWeights $ vote
    where vote = take k $ sortBy (flip $ comparing snd) (map (\(c, d) -> (c, 1/d)) dc)

-- Accuracy percent
accuracy :: [String] -> [Flower] -> Float
accuracy preds tests = (fromIntegral $ length corrects) / (fromIntegral $ length tests)
    where corrects = filter (\(p, t) -> p == t) $ zip preds (map clas tests)

-- Lost percent
lost :: [String] -> [Flower] -> Float
lost p t = 1.0 - (accuracy p t)

{-
kNN algorithm
-------------
Parameters: Set of observations, flower to predict, k, distance function, vote function
Returns: Belonging flower class
-}
kNN :: [Flower] -> Flower -> Int -> (Flower -> Flower -> Float) -> (Int -> [(String, Float)] -> String) -> String
kNN l f k df vf = vf k (calcDistsClass f df l)

-- Get accuracy and lost evaluations
evalKNN :: [String] -> [Flower] -> [([String] -> [Flower] -> Float)] -> [Float]
evalKNN _ _ [] = []
evalKNN p t (f:l) = [f p t] ++ evalKNN p t l

-- Full dataset to list of flowers
readFlowers :: [String] -> [Flower]
readFlowers [] = []
readFlowers (f:l) = [castFlower f] ++ readFlowers l

-- String data to Flower Structure
castFlower :: String -> Flower
castFlower f = Flower (s!!4) (read $ s!!0) (read $ s!!1) (read $ s!!2) (read $ s!!3)
    where s = splitOn "," f

main = do
    -- Train dataset
    trainFile <- readFile "./iris.train.txt"
    let trainset = readFlowers $ lines trainFile
    -- Test dataset
    testFile <- readFile "./iris.test.txt"
    let testset = readFlowers $ lines testFile
    -- Predictions
    let vt = map (\f -> kNN trainset f 3 euclideanDistance simpleVote) testset
    -- Results
    let acc = accuracy vt testset
    let los = lost vt testset
    let res = ["*****",
               "Accuracy: " ++ (showFFloat (Just 2) acc ""),
               "Lost: " ++ (showFFloat (Just 2) los ""),
               "*****"]

    mapM_ putStrLn vt
    mapM_ putStrLn res
