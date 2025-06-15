--fatorar n | 
    -- achar os divisores primeos de n 
    -- calcular proximo primo


-- calcular divisores de um numero


ehNumeroPerfeito n = if ((sum (divi n)) - n) == n then True else False

divi::Int -> [Int]
divi n = divoC n 1

divoC::Int -> Int -> [Int]
divoC n x
    | x == n = [n]
    | x > n  = []
    | mod n x  == 0 = x : divoC n (x+1)
    | otherwise = divoC n (x+1)


fatorar n = diviFat n 2

diviFat n m | m > n                       = []
            | ehPrimo m && (mod n m == 0) =  m : diviFat (div n m) (m)  -- repete o mesmo primo
            | otherwise                   =  diviFat n (m+1)

ehPrimo n =  length (divi n)  == 2

transpor :: [Int] -> [Int] -> [[Int]]
transpor [] [] = []
transpor (a:as) (b:bs) = [a,b] : transpor as bs

--transporVe :: [[Int]] -> [[Int]]


linhaValida [] = True
linhaValida (a:as) = if contOcorrencia a (a:as) == 1 then linhaValida as else False

contOcorrencia a [] = 0
contOcorrencia a (x:xs) = if a == x then 1 + contOcorrencia a (xs) else contOcorrencia a xs

-- funcao que gera uma lista de tuplas de numero de vez que aparece e o numero em si

-- fibonacci eficiente
fibEfi n m o | 
fib n | n <=2 = 1
      | otherwise = fib (n-1) + fib (n-2)
