data Arv = Nulo |
           No Int Arv Arv
            deriving Show

emOrdem :: Arv -> [Int]
emOrdem Nulo = []
emOrdem (No x esq dir) = (emOrdem esq) ++ [x] ++ (emOrdem dir)

preOrdem :: Arv -> [Int]
preOrdem Nulo = []
preOrdem (No x esq dir) = [x] ++ (preOrdem esq) ++ (preOrdem dir)

posOrdem :: Arv -> [Int]
posOrdem Nulo = []
posOrdem (No x esq dir) = (posOrdem esq) ++ (posOrdem dir) ++ [x]


