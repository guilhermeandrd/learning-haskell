--funcao que remove maior
maior [] = []
maior (a:as)
    | m == a = tail (a:as)
    | otherwise = a : maior (as)
    where
        m = maximum (a:as) 
--coloca o maximo no comeco
-- faz um filter com o maximo
-- coloca maximo no comeco
-- retorna

pushMaior (a:as) 
    | m == a = tail (a:as)
    | otherwise = a : pushMaior (as)
    where
        m = maximum (a:as) 
        
