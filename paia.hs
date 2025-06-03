
-- retorna lista [m, m+1, m+2, .. , m+n-1]
sequencia :: Int -> Int-> [Int]
sequencia 0 m = []

sequencia n m = m : sequencia (n-1) (m+1)
rotEsq :: Int -> String -> String
rotEsq 0 [] = []
rotEsq 0 (a:as) = (a:as)
rotEsq n [] = []

rotEsq n (a:as)  = rotEsq (n-1) ls
    where 
        ls = as ++ [a] -- colocar primeiro elemento no final

rotDir :: Int -> String -> String
rotDir 0 [] = []
rotDir 0 (a:as) = (a:as)
rotDir n [] = []

rotDir n (a:as)  = rotDir (n-1) ls
    where 
        ls = last (a:as) : init (a:as) -- colocar ultimo elemento no comeco

quadperf :: Int -> Bool
quadperf n = quadperf' n 1 

quadperf' :: Int -> Int -> Bool
quadperf' n i
    | verquad i n == True = True
    | i*i > n = False
    | otherwise = quadperf' n (i+1)

verquad :: Int -> Int -> Bool
verquad i n 
    | i*i == n = True
    | otherwise = False

deletee n [] = []
deletee n (a:as) 
    | a == n = as
    | otherwise = a : deletee n as


sdig n = contsidg 0 n;

contsidg :: Int -> Int -> Int
contsidg m n
    | n <= 0 = m
    | otherwise = contsidg  (m + (n `rem` 10)) (n `div` 10)

rev n = contn 0 n

contn :: Int -> Int -> Int
contn m n
    | n <= 0 = m
    | otherwise = contn (m * 10 + n `rem` 10) (n `div` 10)
