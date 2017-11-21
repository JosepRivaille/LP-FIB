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

euclideanDistance :: Flower -> Flower -> Float
euclideanDistance f1 f2 = sqrt $ sdSepL + sdSepW + sdPetL + sdPetW
    where
        sdSepL = (sepL f1 - sepL f2) ** 2
        sdSepW = (sepW f1 - sepW f2) ** 2
        sdPetL = (petL f1 - petL f2) ** 2
        sdPetW = (petW f1 - petW f2) ** 2

manhattanDistance :: Flower -> Flower -> Float
manhattanDistance f1 f2 = adSepL + adSepW + adPetL + adPetW
    where
        adSepL = abs $ (sepL f1) - (sepL f2)
        adSepW = abs $ (sepW f1) - (sepW f2)
        adPetL = abs $ (petL f1) - (petL f2)
        adPetW = abs $ (petW f1) - (petW f2)

calcDistsClass :: Flower -> (Flower -> Flower -> Float) -> [Flower] -> [(String, Float)]
calcDistsClass f df = map (\x -> (clas x, df f x))

countAppearances :: [String] -> (String, Int) -> [(String, Int)]
countAppearances [e] current@(clas, count)
    | e == clas = [(clas, count + 1)]
    | otherwise = [current, (e, 1)]
countAppearances (e:l) current@(clas, count)
    | e == clas = countAppearances l (clas, count + 1)
    | otherwise = [current] ++ countAppearances l (e, 1)

sumWeights :: [(String, Float)] -> (String, Float) -> [(String, Float)]
sumWeights [(c, w)] current@(clas, weight)
    | c == clas = [(clas, weight + w)]
    | otherwise = [current, (c, w)]
sumWeights ((c, w):l) current@(clas, weight)
    | c == clas = sumWeights l (clas, weight + w)
    | otherwise = [current] ++ sumWeights l (c, w)

maxAppearances :: [String] -> String
maxAppearances l = fst $ head $ sortBy (flip $ comparing snd) (countAppearances sl (head sl, 0))
    where sl = sort l

maxWeights :: [(String, Float)] -> String
maxWeights l = fst $ head $ sortBy (flip $ comparing snd) (sumWeights sl (head sl))
    where sl = sortBy (comparing fst) l

-- Falta arreglar cas empat!
simpleVote :: Int -> [(String, Float)] -> String
simpleVote k dc = maxAppearances $ map fst $ vote k dc

weightedVote :: Int -> [(String, Float)] -> String
weightedVote k dc = maxWeights $ map (\(c, d) -> (c, 1/d)) (vote k dc)

vote :: Int -> [(String, Float)] -> [(String, Float)]
vote k dc = take k $ sortBy (comparing snd) dc

accuracy :: [String] -> [Flower] -> Float
accuracy preds tests = (fromIntegral $ length corrects) / (fromIntegral $ length tests)
    where corrects = filter (\(p, t) -> p == t) $ zip preds (map clas tests)

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

evalKNN :: [String] -> [Flower] -> [([String] -> [Flower] -> Float)] -> [Float]
evalKNN _ _ [] = []
evalKNN p t (f:l) = [f p t] ++ evalKNN p t l

readFlowers :: [String] -> [Flower]
readFlowers [] = []
readFlowers (f:l) = [castFlower f] ++ readFlowers l

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
    let vt = map (\f -> kNN trainset f 1 euclideanDistance simpleVote) testset
    -- Results
    let acc = accuracy vt testset
    let los = lost vt testset
    let res = ["*****",
               "Accuracy: " ++ (showFFloat (Just 4) acc ""),
               "Lost: " ++ (showFFloat (Just 4) los ""),
               "*****"]

    mapM_ putStrLn vt
    mapM_ putStrLn res
