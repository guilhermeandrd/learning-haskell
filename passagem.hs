import Distribution.Compat.CharParsing (CharParsing(string))
passagem :: Double -> Double -> Double

passagem a b | b > 59 = a * 0.6
             | b < 11 && b > 2 = a * 0.5
             | b <= 2 = a * 0.1
             | otherwise = a

-- otherwise para representar outros casos
