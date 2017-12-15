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

-- Splits a sequence by an element
splitOn :: Ord a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn sep s = [takeWhile (/= sep) s] ++ splitOn sep trs
    where
        trs = if null rs then [] else tail rs
        rs = dropWhile (/= sep) s

-- Sorts a list by a comparing criteria
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy cmp (x:l) = (sortBy cmp left) ++ [x] ++ (sortBy cmp right)
    where
        left = [e | e <- l, cmp e x /= GT]
        right = [e | e <- l, cmp e x == GT]

-- Gives a comparing criteria for generic types
comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing f x y = compare (f x) (f y)

-- ***** DISTANCES *****

-- Euclidean distance given two flowers
euclideanDistance :: DistanceFun
euclideanDistance (Flower _ s1) (Flower _ s2) = sqrt.sum.map (**2) $ zipWith (-) s1 s2

-- Manhattan distance given two flowers
manhattanDistance :: DistanceFun
manhattanDistance (Flower _ s1) (Flower _ s2) = sum.map abs $ zipWith (-) s1 s2

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
readFlowers l = Dataset $ map castFlower l

-- String data to Flower Structure
castFlower :: String -> Flower
castFlower f = Flower (s!!4) [(read $ s!!0), (read $ s!!1), (read $ s!!2), (read $ s!!3)]
    where s = splitOn ',' f

-- ***** Input / Output *****

-- Asks user and returns k
askK :: IO String
askK = do
    putStrLn "\nChoose k parameter:"
    getLine

-- Asks user and returns 1 (Euclidean distance) or 2 (Manhattan distance)
askDistanceFun :: IO String
askDistanceFun = do
    putStrLn "\nChoose distance function:"
    putStrLn "1. Euclidean distance."
    putStrLn "2. Manhattan distance."
    df <- getLine
    if df == "1" || df == "2"
    then return df
    else askDistanceFun

-- Asks user and returns 1 (Simple vote) or 2 (Weighted vote)
askVoteFun :: IO String
askVoteFun = do
    putStrLn "\nChoose vote function:"
    putStrLn "1. Simple vote."
    putStrLn "2. Weighted vote."
    vf <- getLine
    if vf == "1" || vf == "2"
    then return vf
    else askVoteFun

-- Asks user to restart KNN
askToRestart :: IO ()
askToRestart = do
    putStrLn "\nRepeat with new functions?"
    newExp <- getLine
    if elem newExp ["Yes", "YES", "yes", "Y", "y"]
    then main
    else putStrLn "Program finished."

-- ***** MAIN *****

main :: IO ()
main = do
    -- Train dataset
    trainFile <- readFile "./iris.train.txt"
    let trainset = readFlowers $ lines trainFile
    -- Test dataset
    testFile <- readFile "./iris.test.txt"
    let testset@(Dataset testsetlist) = readFlowers $ lines testFile

    -- Ask for parameters k, distance function and vote function
    k <- askK
    dfq <- askDistanceFun
    let df = [euclideanDistance, manhattanDistance] !! (read dfq - 1)
    vfq <- askVoteFun
    let vf = [simpleVote, weightedVote] !! (read vfq - 1)

    -- Predictions
    let originals = map (\(Flower c _) -> c) testsetlist
    let predictions = map (\f -> kNN trainset f (read k) df vf) testsetlist
    let zippingPred o p = (init o) ++ "\t->\t" ++ (init p) ++ "\t->\t" ++ if o == p then "O" else "X"
    putStrLn "\nTestset -> Prediction -> Matching (O) | Non-matching (X)"
    putStrLn "*********************************************************"
    mapM_ putStrLn $ zipWith zippingPred originals predictions

    -- Evaluate functions
    let res = evalKNN predictions testset [accuracy, lost]
    let mappingEval (t, x) = t ++ ": " ++ (showFFloat (Just 2) x "")
    mapM_ putStrLn $ map mappingEval $ zip ["\nAccuracy", "Lost"] res

    -- Reset program if necessary
    askToRestart
