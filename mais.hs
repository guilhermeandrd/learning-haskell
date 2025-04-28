mais :: Char -> [Char]
mais ch = [snd t | t<- xy, fst t==ch]
    where
        x = ['a'..'z']
        y = ['A'..'Z']
        xy = zip x y


mais' gh = [snd t | t <- xy, fst t==gh]
    where
        x = ['a'..'z']
        y = ['A'..'Z']
        xy = zip x y