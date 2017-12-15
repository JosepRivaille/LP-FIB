serieCollatz :: Integer -> [Integer]
serieCollatz n = takeWhile (/= 1) serie ++ [1]
    where serie = iterate (\x -> if mod x 2 == 0 then div x 2 else x * 3 + 1) n

collatzMesLlarga :: Integer -> Integer
collatzMesLlarga n = maximum $ map (toInteger . length . serieCollatz) [1..n]

representantsCollatz :: [Integer] -> [Integer]
representantsCollatz = map (toInteger . length . serieCollatz)
