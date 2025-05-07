soma ls | [] = 0
        | length ls == 1 = head ls
        | otherwise = head ls + soma (tail ls)


reducao ls f | length ls == 0 = 0
             | length ls == 1 = head ls
             | otherwise = head ls f reducao (tail ls)