--fib' xs@(a:b:ls) = [a] ++ fib' b (a+b)

--funcao fibonacci
--retorna uma lista infinita da sequencia fibonacci
--uso : take n fib
--n sendo um numero integral, retorna um pedaço da lista infinita
fib a b = [a] ++ fib b (a+b)

--funcao count
--retorna quantas vezes um elemento aparece na lista
--uso : count lista elemento
count [] _ = 0
count (a:ls) x = if x == a then 1 + count ls x else count ls x

--fazer uma lista alternada do numero 1 de negativos e positovs ate o numero passado como parametro na funcao
-- uso : par elemento
par n = if n < 1 then [] else par (n-1) ++ [-n,n]

--quick sort em 5 linhas
--uso : quickSort [ls]
quickSort [] = []
quickSort [a] = [a]
quickSort (p:ls) = menores ++ [p] ++ maiores
        where
            menores = quickSort [c | c <- ls, c<p]
            maiores = quickSort [c | c <- ls, c>p]

--fazer o merge sort
--selection [] = []
--selection [a] = [a]
--selection (a:ls) = [a] 


-- funcao unique iterativa
--verifica se um numero na lista eh unico ou nao, retornando True se for unico e False se não for
--fazer
-- uso : unique [ls] n
uniqueInt ls n = length (filter (== n) ls) == 1

-- funcao unique recursiva que usa cont
uniqueRec [] n cont = cont == 1
uniqueRec ls n cont = 
    if (head ls) == n then uniqueRec (tail ls) n (cont+1)
    else uniqueRec (tail ls) n (cont)

-- funcao unique de interface que usa unique recursiva debaixo dos panos
unique ls n = uniqueRec ls n 0

-- funcao unique que usa o count
uniqueCont ls n = count ls n == 1

--funcao simples que retorna uma lista de unicos
--usar unique no resto da lista usando head 
unico [] = []
unico (a:as) = if unique (a:as) a then a : unico (as) else unico (as)

excluidor [] = []
excluidor (a:as) = if (count (a:as) a) > 1 then excluidor (filter (/=a) (a:as)) else a:excluidor (as)

diferenca ls xs = excluidor (ls ++ xs)

--criar uma nova lista dos dois
-- retornar uma lista de unicos

--ocorrencias [] = []
--ocorrencias (a:xs) = zip OcorrenciasCount (a:xs) OcorrenciasNum (a:xs)

ocorrenciasCount [] = []
ocorrenciasCount (a:xs) = (a, count (a:xs) a) : ocorrenciasCount (filter (/=a) xs)


maisComum [] = 0
--maisComum (x:xs) =

maioresQue n [] = []
maioresQue n (a:as) = if a > n then a : maioresQue n (as) else maioresQue n (as)

concatena [] [] = []
concatena (x:xs) [] = (x:xs)
concatena [] (x:xs) = (x:xs)
concatena (x:xs) (y:ys) = x : concatena (xs) (y:ys)
