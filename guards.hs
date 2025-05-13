imc :: Float -> Float -> String
imc p h
    | i < 18.5  = "magro"
    | i < 28.5  = "normal"
    | otherwise = "gordo"
  where
    i = p / (h * h)


--triangulos se existe e seu tipo
-- se é acutangulo
-- se existe
-- se é

tri :: int -> Int -> Int -> String

tri a b c | existe && maior^2 == bg^2 + cg^2  = "existe e eh retangulo"
          | existe && (maior^2 < bg^2 + cg^2)  = "existe e eh acutangulo"
          | existe && (maior^2 > bg^2 + cg^2) = "existe e eh obtusangulo"
          | otherwise = "nao existe"
    where
        existe = a < b+c && a > negate b-c || b < a+c && b > negate a-c || c < a+b && c > negate a-b
        ls = a : b : [c]
        maior = maximum ls
        bg = ((sum ls) - maior) `div` 2
        cg = sum ls - maior - bg


diff a b c
    | a > b && a > c = a*a-b*b-c*c
    | b > c = b * b - a * a - c* c
    | otherwise = c*c-a*a-b*b
     
existe a b c = a<b+c && a > abs (b-c)
 
triang a b c
    | e && (d==0) = "existe e eh retangulo"
    | e && (d<0)  = "existe e eh acutangulo"
    | e && (d>0)  = "existe e eh obstusangulo"
    | otherwise = "o triangulo nao existe"
    where
        e = existe a b c
        d = diff a b c
