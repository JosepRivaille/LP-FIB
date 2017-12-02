import Data.List

mult :: [Double] -> [Double] -> [[Double]]
mult p1 p2 = transpose [p1, p2]
