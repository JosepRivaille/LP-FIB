import System.IO

getInterpretation :: Double -> String
getInterpretation imc
    | imc < 18.0    = "magror"
    | imc < 25.0    = "corpulencia normal"
    | imc < 30.0    = "sobrepes"
    | imc < 40.0    = "obesitat"
    | otherwise     = "obesitat morbida"

calculateIMC :: String -> String
calculateIMC l = name ++ ": " ++ getInterpretation imc
    where
        [name, weight, height] = words l
        imc = (read weight) / ((read height) ** 2)

main = do
    l <- getLine
    if l == "*"
    then return()
    else putStrLn $ calculateIMC l
    main
