import Data.Ord
import Data.List

votsMinim :: [([Char], Int)] -> Int -> Bool
votsMinim l v = any (\(_, cv) -> cv < v) l

candidatMesVotat :: [([Char], Int)] -> [Char]
candidatMesVotat l = fst $ maximumBy (comparing snd) l

votsIngressos :: [([Char], Int)] -> [([Char], Int)] -> [[Char]]
votsIngressos vl il = [n | (n, _) <- vl, not $ elem n $ map fst il]

rics :: [([Char], Int)] -> [([Char], Int)] -> [[Char]]
rics vl il = map (astIfCand) $ map fst $ take 3 $ sortBy (flip $ comparing snd) il
    where
        astIfCand :: [Char] -> [Char]
        astIfCand n
            | elem n $ map fst vl   = n ++ ['*']
            | otherwise             = n
