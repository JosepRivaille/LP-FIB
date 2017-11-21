fizzBuzz :: [Either Int String]
fizzBuzz = map fb [0..]

fb :: Int -> Either Int String
fb n
    | mod n 3 == 0 && mod n 5 == 0  = Right "FizzBuzz"
    | mod n 3 == 0                  = Right "Fizz"
    | mod n 5 == 0                  = Right "Buzz"
    | otherwise                     = Left n
