palin [] = []
palin (a:as) 
    | ehPalin a = a : palin (as)
    | otherwise = palin (as)

ehPalin [] = True
ehPalin [a] = True
ehPalin (a:as)
    | a == last (a:as) = ehPalin (xs)
    | otherwise = False
    where 
        xs = init (tail (a:as))
