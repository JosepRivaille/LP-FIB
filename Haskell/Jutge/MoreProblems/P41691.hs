import Data.List

prefsufs :: [Double] -> [[Double]]
prefsufs l = prefs ++ suffs
    where
        len = length l
        prefs = map (\i -> take i l) [1..len]
        suffs = map (\i -> drop i l) [1..(len-1)]

mult :: [Double] -> [Double] -> [Double]
mult p1 p2 = let psx = prefsufs p1
                 psy = map reverse $ prefsufs p2
             in  (zipWith (\xi yi -> sum $ zipWith (*) xi yi) psx psy) ++ [0.0]
