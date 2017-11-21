import Data.Ord
import Data.List

data Avi = Avi {
    nom :: [Char],
    edat :: Int,
    despeses :: [Int]
} deriving (Show)

average :: [Int] -> Int
average l = round $ (fromIntegral (sum l)) / (fromIntegral (length l))

promigDespeses :: Avi -> Int
promigDespeses (Avi _ _ d) = average d

edatsExtremes :: [Avi] -> (Int, Int)
edatsExtremes avis = (minAge, maxAge)
    where
        minAge = edat $ minimumBy (comparing edat) avis
        maxAge = edat $ maximumBy (comparing edat) avis

sumaPromig :: [Avi] -> Int
sumaPromig avis = sum [promigDespeses a | a <- avis]

maximPromig :: [Avi] -> Int
maximPromig avis = maximum [promigDespeses a | a <- avis]

despesaPromigSuperior :: [Avi] -> Int -> ([Char], Int)
despesaPromigSuperior avis thresh
    | length redList == 0 = ("", 0)
    | otherwise           = nameAge $ head redList
    where
        redList = sortBy (comparing promigDespeses) [avi | avi <- avis, promigDespeses avi > thresh]
        nameAge a = (nom a, edat a)
