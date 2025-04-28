mais' :: Char -> Char
mais' ch = if not (null ls) then head ls else ch
    where
        ls = [snd t | t <- xy, fst t == ch]
        x = ['a'..'z']
        y = ['A'..'Z']
        xy = zip x y

capWord :: [Char] -> [Char]
capWord w = mais'  h : t
    where
        h = head w
        t = tail w

cap :: [Char] -> String
cap xs = unwords ls
    where 
        ws = words xs
        ls = [capWord w | w <- ws]

--quero fazer uma função que capitalize uma palavra inteira
--capXs :: String -> String
--capXs st = 

--acho que primeiro vou fazer uma função que ver se a palavra tem alguma letra minuscula
