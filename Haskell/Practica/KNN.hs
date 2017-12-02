import System.IO
import Numeric

data Flower = Flower String [Float] deriving (Show)
data Dataset = Dataset [Flower] deriving (Show)

data ClassDistance = ClassDistance {
    dclas :: String,
    distance :: Float
} deriving (Show)

data ClassCount = ClassCount {
    cclas :: String,
    count :: Int
} deriving (Show)

data ClassWeight = ClassWeight {
    wclas :: String,
    weight :: Float
} deriving (Show)

type DistanceFun = Flower -> Flower -> Float
type VotationFun = Int -> [(ClassDistance)] -> String
type EvaluationFun = [String] -> Dataset -> Float

-- ***** HELPERS *****

splitOn :: Ord a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn sep s = [takeWhile (/= sep) s] ++ splitOn sep trs
    where
        trs = if null rs then [] else tail rs
        rs = dropWhile (/= sep) s

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy cmp (x:l) = (sortBy cmp left) ++ [x] ++ (sortBy cmp right)
    where
        left = [e | e <- l, cmp e x /= GT]
        right = [e | e <- l, cmp e x == GT]

comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing f x y = compare (f x) (f y)

-- ***** DISTANCES *****

-- Euclidean distance given two flowers
euclideanDistance :: DistanceFun
euclideanDistance (Flower _ s1) (Flower _ s2) = sqrt . sum . map (**2) $ zipWith (-) s1 s2

-- Manhattan distance given two flowers
manhattanDistance :: DistanceFun
manhattanDistance (Flower _ s1) (Flower _ s2) = sum . map abs $ zipWith (-) s1 s2

-- Evaluates a distance function given a flower and a list of flowers
calcDistsClass :: Flower -> DistanceFun -> Dataset -> [ClassDistance]
calcDistsClass f dfun (Dataset l) = map (\f'@(Flower c _) -> (ClassDistance c (dfun f f'))) l

-- ***** VOTATION *****

-- Reduced list with sum of same-class scoress and number of appearances given ordered list
countAppearances :: [ClassCount] -> [ClassCount]
countAppearances [] = []
countAppearances [e] = [e]
countAppearances (e1:e2:l)
    | cclas e1 == cclas e2  = [e'] ++ countAppearances (e':l)
    | otherwise             = [e1] ++ countAppearances (e2:l)
        where e' = (ClassCount (cclas e2) (count e2 + count e1))

-- Reduced list with sum of same-class scores given ordered list
sumWeights :: [ClassWeight] -> [ClassWeight]
sumWeights [] = []
sumWeights [e] = [e]
sumWeights (e1:e2:l)
    | wclas e1 == wclas e2  = [e'] ++ sumWeights (e':l)
    | otherwise             = [e1] ++ sumWeights (e2:l)
        where e' = (ClassWeight (wclas e2) (weight e2 + weight e1))

-- Class corresponding to max appearing class
maxAppearances :: [String] -> String
maxAppearances l = cclas $ head $ sortBy (flip $ comparing count) appearances
    where appearances = countAppearances $ map (\c -> (ClassCount c 1)) $ sortBy compare l

-- Class corresponding to greater weighted score
maxWeights :: [ClassWeight] -> String
maxWeights l = wclas $ head $ sortBy (flip $ comparing weight) weights
    where weights = sumWeights $ sortBy (comparing wclas) l

-- Simple vote for kth greater scores
simpleVote :: VotationFun
simpleVote k dc = maxAppearances $ map dclas $ vote
    where vote = take k $ sortBy (comparing distance) dc

-- Weighted vote for kth greater scores
weightedVote :: VotationFun
weightedVote k dc = maxWeights $ vote
    where
        vote = take k $ sortBy (flip $ comparing weight) $ invertedW
        invertedW = map (\(ClassDistance c d) -> (ClassWeight c (1/d))) dc

-- ***** EVALUATION *****

-- Accuracy percent
accuracy :: EvaluationFun
accuracy preds (Dataset tests) = (fromIntegral $ length corrects) / (fromIntegral $ length tests)
    where corrects = filter (\(p, t) -> p == t) $ zip preds $ map (\(Flower c _) -> c) tests

-- Lost percent
lost :: EvaluationFun
lost preds testset = 1.0 - (accuracy preds testset)

{-
kNN algorithm
-------------
Parameters: Set of observations, flower to predict, k, distance function, vote function
Returns: Belonging flower class
-}
kNN :: Dataset -> Flower -> Int -> DistanceFun -> VotationFun -> String
kNN ds f k df vf = vf k (calcDistsClass f df ds)

-- Get accuracy and lost evaluations
evalKNN :: [String] -> Dataset -> [EvaluationFun] -> [Float]
evalKNN p ds = map (\f -> f p ds)

-- Full dataset to list of flowers
readFlowers :: [String] -> Dataset
readFlowers [] = (Dataset [])
readFlowers (f:l) = (Dataset ([castFlower f] ++ flowers))
    where (Dataset flowers) = readFlowers l

-- String data to Flower Structure
castFlower :: String -> Flower
castFlower f = Flower (s!!4) [(read $ s!!0), (read $ s!!1), (read $ s!!2), (read $ s!!3)]
    where s = splitOn ',' f

main :: IO ()
main = do
    -- Train dataset
    trainFile <- readFile "./iris.train.txt"
    let trainset = readFlowers $ lines trainFile
    -- Test dataset
    testFile <- readFile "./iris.test.txt"
    let testset@(Dataset testsetlist) = readFlowers $ lines testFile
    -- Distance functions
    let dfs = [euclideanDistance, manhattanDistance]
    -- Vote functions
    let vfs = [simpleVote, weightedVote]
    -- Evaluate functions
    let efs = [[accuracy], [lost], [accuracy, lost]]

    putStrLn "Choose k parameter:"
    k <- getLine

    putStrLn "Choose distance function:"
    putStrLn "1. Euclidean distance."
    putStrLn "2. Manhattan distance."
    dfq <- getLine

    putStrLn "Choose vote function:"
    putStrLn "1. Simple vote."
    putStrLn "2. Weighted vote."
    vfq <- getLine

    -- Predictions
    let df = dfs !! (read dfq - 1)
    let vf = vfs !! (read vfq - 1)
    let vt = map (\f -> kNN trainset f (read k) df vf) testsetlist

    mapM_ putStrLn vt
    putStrLn "**********"

    putStrLn "Choose evaluation function:"
    putStrLn "1. Accuracy"
    putStrLn "2. Lost"
    putStrLn "3. Both"
    efq <- getLine

    let ef = efs !! (read efq - 1)
    let res = evalKNN vt testset ef
    mapM_ putStrLn $ map (\x -> showFFloat (Just 2) x "") res

    putStrLn "\nRepeat with new functions? (Y/N)"
    newExp <- getLine
    putStrLn "**********"

    if newExp == "Y" then main else putStrLn "Program finished."
