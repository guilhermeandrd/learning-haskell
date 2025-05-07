mais :: Char -> [Char]
mais ch = [snd t | t<- xy, fst t==ch]
    where
        x = ['a'..'z']
        y = ['A'..'Z']
        xy = zip x y


mais' ch = if not (null ls) then head ls else ch
    where
        ls = [snd t | t <- xy, fst t == ch]
        x = ['a'..'z']
        y = ['A'..'Z']
        xy = zip x y


capWord w = mais'  h : t
    where
        h = head w
        t = tail w
cap xs = unwords ls
    where 
        ws = words xs
        ls = [capWord w | w <- ws]

--maneira eficiente
capitalizar [] = [] --(caso base)
capitalizar [x] = cap x
capitalizar (x:xs) = x + capitalizar xs
--(somall é o primeiro elemento + o somall do resto)

