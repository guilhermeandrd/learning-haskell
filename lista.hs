
data Lista a = Vazia | No a (Lista a) deriving (Show, Read)

-- inserir
-- remover
-- buscar

inserir key ls = No key ls

lsprint' Vazia = ""
lsprint' (No key ls) = show key ++ "," ++ lsprint' ls

lsprint ls = "[" ++ init (lsprint' ls) ++ "]"

-- procurar o no que queremos remover e removelo
remover key ls = ls

--printa :: Lista -> Lista
--printa ls = read ls :: (Lista Int)

head' Vazia = error "lista vazia"
head' (No x _) = x

tail' Vazia = Vazia
tail' (No _ ls) = ls

last' Vazia = error "lista vazia"
last' (No x Vazia) = x
last' (No _ ls) = last' ls

init' Vazia = Vazia
init' (No _ Vazia) = Vazia
init' (No x ls) = No x (init' ls)

--strtols s =
  --  where (x, y) = break s
