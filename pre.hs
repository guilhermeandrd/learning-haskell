mdc a b = if b == 0 then a else mdc b (mod a b)

data Frac = Frac Int Int

instance Show Frac where
    show (Frac n d) = "(" ++ show (n `div` m) ++ "/" ++ show (d `div` m) ++ ")"
        where m = mdc n d

instance Num Frac where -- consertar isso aqui
    (+) (Frac a m) (Frac b n) = Frac num g
        where
            g = m * n -- g eh denominador
            num = ((div g m) * a +  (div g n) * b)
    (-) (Frac a m) (Frac b n) = Frac num g
        where
            g = m * n -- g eh denominador
            num = ((div g m) * a -  (div g n) * b)
    (*) (Frac a m) (Frac b n) = Frac num g
        where
            g = m * n -- g eh denominador
            num = a * b
-- fazer de multiplicao

-- divisao

-- subtracao

   
