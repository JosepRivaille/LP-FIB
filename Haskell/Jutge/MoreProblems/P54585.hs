euclidean :: (Float, Float) -> (Float, Float) -> Float
euclidean (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

closest :: [(Float,Float)] -> Float
closest (e1:e2:[]) = euclidean e1 e2
closest (e:l) = minimum $ map (\p -> euclidean e p) l ++ [closest l]
