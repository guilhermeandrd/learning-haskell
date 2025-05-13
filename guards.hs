imc :: Float -> Float -> String
imc p h
    | i < 18.5  = "magro"
    | i < 28.5  = "normal"
    | otherwise = "gordo"
  where
    i = p / (h * h)


--triangulos se existe e seu tipo
-- se é acutangulo
-- se existe
-- se é
