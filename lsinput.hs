contar :: [Int] -> Int

contar ls n =
    length [x|x<-ls, x==n]
    
