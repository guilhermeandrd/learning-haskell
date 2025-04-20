ff x = x*x

sqtri p = sum [1 | a <- [1..], b <- [1..a], c <- [1..b], a**2==b**2+c**2]


-- as listas irem até p e a restrição ser a+b+c<p

sqtri'2 p = sum [1 | a <- la, b <- lb, c <- lc, a**2==b**2+c**2]
    where
        la = [1..]
        lb = [1..a]
        lc = [1..b]
   
-- la, lb, lc retornam listas
