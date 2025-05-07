
teste :: Int -> Int  -> Int
teste a b = a+b

reduce ls f =
   if length ls <=1 then
     head ls
   else
     f (head ls) (reduce (tail ls) f)