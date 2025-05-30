--funcao que alterna numero entre negativo e positivo ateh o numero

alter n = zip ls ns
    where 
        ls = [1..n]
        ns = map negate ls

alterna :: Int -> [Int]
alterna 0 = []

alterna n = alterna (n-1) ++ [n, -n]


--funcao que retorna lista invertida
rev :: [Int] -> [Int]
rev [] = []

rev (a:as) = last (a:as) : rev (init (a:as))

--12345
--51234

--1234
--4123

--123
--312

--12
--21


concatena :: [Int] -> [Int] -> [Int]

concatena [] []  = []
concatena (a:as) [] = (a:as)
concatena [] (x:xs) = (x:xs)
concatena (a:as) (x:xs) = a : concatena as (x:xs)

--funcao que retorna n menores da lista

menor 0 [] = []

menor 0 (a:as) = []

menor n [] = []

--verifica se cada elemento eh um menor dentro do espaco designado
--caso sim coloca
menor n (a:as) 
    | ehMinor n a (a:as) = a : menor (n-1) as
    | otherwise = menor n as 

--1, 2, 3, 3, 4
--1, 2, 3, 3

--funcao auxiliar que dentre n ver se m tah no espaco designado
ehMenor n m i ls
    | m == minimum ls = True
    | n == 0 && m /= minimum ls = False
    | ls == [] = False
    | otherwise = ehMenor n m (i+1) xs
  where
    (parte1, parte2) = splitAt i ls
    xs = if null parte1 
            then tail parte2
         else if null parte2
            then parte1
         else parte1 ++ tail parte2

ehMinor n m (a:as)
    | length xs <= n = True
    | otherwise = False
    where
        xs = filter (<= m) (a:as)
--to tendo problema com o indice

--caso base ehMenor 



