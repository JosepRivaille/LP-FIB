mult :: [Double] -> [Double] -> [[Double]]
mult p1 p2 = map (\c -> map (*c) p2) p1
