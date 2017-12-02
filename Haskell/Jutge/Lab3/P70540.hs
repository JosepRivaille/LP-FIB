data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val v) = v
eval1 (Add e1 e2) = (eval1 e1) + (eval1 e2)
eval1 (Sub e1 e2) = (eval1 e1) - (eval1 e2)
eval1 (Mul e1 e2) = (eval1 e1) * (eval1 e2)
eval1 (Div e1 e2) = div (eval1 e1) (eval1 e2)

eval2 :: Expr -> Maybe Int
eval2 (Val v) = Just v
eval2 (Add e1 e2) = Just $ (eval2 e1) + (eval2 e2)

eval2 (Sub e1 e2) = Just $ (eval2 e1) - (eval2 e2)
    where
eval2 (Mul e1 e2) = Just $ (eval2 e1) * (eval2 e2)
eval2 (Div e1 e2)
    | eval2 e2 == 0 = Nothing
    | otherwise     = Just $ div (eval2 e1) (eval2 e2)
