teste :: Int -> Int -> Int
teste a b = a+b

reduce ls f =
    if length ls <1 then
        error "voce silas"
    else 
        f (head ls) (reduce (tail ls))
        
--funcao lambda
