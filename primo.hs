--divisores primos
pdivs x = [d|d <- [2..x], d**2<x, mod x d==0]

eprimo x = length (pdivs x) == 0

takeprimos n = take n [x| x<-[2..], eprimo x]

