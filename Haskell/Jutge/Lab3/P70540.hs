data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add x y) = (eval1 x) + (eval1 y)
eval1 (Sub x y) = (eval1 x) - (eval1 y)
eval1 (Mul x y) = (eval1 x) * (eval1 y)
eval1 (Div x y) = div (eval1 x) (eval1 y)

eval2 :: Expr -> Maybe Int
eval2 (Val x) = Just x
eval2 (Add x y) = eval2 x >>= (\x' -> eval2 y >>= (\y' -> Just (x' + y')))
eval2 (Sub x y) = eval2 x >>= (\x' -> eval2 y >>= (\y' -> Just (x' - y')))
eval2 (Mul x y) = eval2 x >>= (\x' -> eval2 y >>= (\y' -> Just (x' * y')))
eval2 (Div x y) = eval2 x >>= (\x' -> eval2 y >>= (\y' -> if y' /= 0
                                                          then Just (div x' y')
                                                          else Nothing))

eval3 :: Expr -> Either String Int
eval3 (Val x) = Right x
eval3 (Add x y) = eval3 x >>= (\x' -> eval3 y >>= (\y' -> Right (x' + y')))
eval3 (Sub x y) = eval3 x >>= (\x' -> eval3 y >>= (\y' -> Right (x' - y')))
eval3 (Mul x y) = eval3 x >>= (\x' -> eval3 y >>= (\y' -> Right (x' * y')))
eval3 (Div x y) = eval3 x >>= (\x' -> eval3 y >>= (\y' -> if y' /= 0
                                                          then Right (div x' y')
                                                          else Left "div0"))
