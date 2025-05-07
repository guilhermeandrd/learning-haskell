ident = [("Guilherme Andrade da Silva"), ("566287")]


-- recebe natural
-- devolve lista de tuplas
-- com fatores primos
-- e suas potencias

-- first a base do primo
-- segundo o expoente da potencia

--12
--(2,2) (3,1)
-- preciso saber os divisores primos


--codigo 01
primo :: Int -> Int
primo a = 
    if a == 1
        then 1
    else
        let
            x = [2..(a-1)]
            y = filter (\x -> a `mod` x == 0) x
        in
            if null y
                then a
                else head y

fatorar :: Int -> [Int]
fatorar a = 
    if a == 1
        then []
    else
        let
            x = primo a
            y = a `div` x
            z = fatorar y
        in
            x : z

tprimos :: Int -> [(Int, Int)]
tprimos n = resultado
    where
        lista = fatorar n
        
        contar :: Int -> [Int] -> Int
        contar _ [] = 0
        contar m (x:xs) = 
            if m == x
                then 1 + contar m xs
                else contar m xs

        remover :: Int -> [Int] -> [Int]
        remover _ [] = []
        remover m (x:xs) =
            if m == x
                then remover m xs
                else x : remover m xs

        agrupar :: [Int] -> [(Int, Int)]
        agrupar [] = []
        agrupar (x:xs) =
            let quantidade = contar x (x:xs)
                resto = remover x xs
            in (x, quantidade) : agrupar resto
        
        resultado = 
            if n == 1
                then []
            else agrupar lista




-- codigo 02
--recebe uma string de lista de char e retorna a quantidade do mesmo elemento e o elemento em tupla
-- exemplo: "aaabbc" -> [('a',3),('b',2),('c',1)]
--primeiro o elemento
--segundo a quantidade

freq :: String -> [(Char, Int)]
freq s = resultado
    where
        lista = s

        contar :: Char -> String -> Int
        contar _ [] = 0
        contar m (x:xs) = 
            if m == x
                then 1 + contar m xs
                else contar m xs

        remover :: Char -> String -> String
        remover _ [] = []
        remover m (x:xs) =
            if m == x
                then remover m xs
                else x : remover m xs

        agrupar :: String -> [(Char, Int)]
        agrupar [] = []
        agrupar (x:xs) =
            let quantidade = contar x (x:xs)
                resto = remover x xs
            in (x, quantidade) : agrupar resto
        resultado = 
            if s == ""
                then []
            else agrupar lista