data Batata = Pedra | Papel | Tesoura


instance Eq Batata where
    (==) Pedra Pedra = True
    (==) Tesoura Tesoura = True
    (==) Papel Papel = True
    (==) _ _ = False

--instance Ord Batata where
  ---  (<) Pedra Papel = True
    --(<) Papel Tesoura = True
    --(<) Tesoura Pedra = True
    --(<) _ _ = False
    --(>) x y = if x /= y then not(x < y) else False

instance Ord Batata where
    compare Pedra Pedra = EQ
    compare Papel Papel = EQ
    compare Tesoura Tesoura= EQ
    compare Pedra Papel = LT
    compare Tesoura Pedra = LT
    compare Papel Tesoura = LT
    compare _ _ = GT
    (>) a b = (compare a b) == GT
    (>=) a b = (compare a b) == GT || (a == b)
    

--instance Ord Batata where
  --  (>=) Pedra Pedra = True:
    --(>=) :

--crivo de erastones
crivo x = crivoRec [2..x]

crivoRec [] = []
crivoRec (a:as) = a : crivoRec (filter (\n -> mod n a /= 0) (as))
