import System.IO

main = do
    name <- getLine
    let l = last name
    if l == 'a' || l == 'A'
    then putStrLn "Hola maca!"
    else putStrLn "Hola maco!"
