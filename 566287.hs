--Guilherme Andrade da Silva - 566287

fibo'list :: Int -> [Integer]
fibo'list m
  | m < 0     = []
  | m == 0    = [0]
  | m == 1    = [0, 1]
  | otherwise = foldl step [0, 1] [2..m]
  where
    step acc _ =
      let n1 = last acc          
          n2 = last (init acc)   
      in acc ++ [n1 + n2]

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys                         
merge xs [] = xs                           
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)        
  | otherwise = y : merge (x:xs) ys 

mergesort :: (Ord a) => [a] -> [a]
mergesort []  = []                          
mergesort [x] = [x]                         
mergesort xs  =
  let (left, right) = splitAt (length xs `div` 2) xs
  in merge (mergesort left) (mergesort right)


