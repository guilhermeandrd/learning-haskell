-- data time
-- construtor
    -- lista encadeada
    ---- usa letra maiuscula
-- retornar esses caras como saidas

data Sinal = Verde | Vermelho | Amarelo

qual'e :: Sinal -> String
qual'e Verde = "Green"
qual'e Vermelho = "Red"
qual'e Amarelo = "Yellow"

mudar :: Sinal -> Sinal
mudar Verde = Amarelo 
mudar Amarelo = Vermelho
mudar Vermelho = Verde


data Bask a = None | Complex | Roots a a deriving Show --letra a eh outro tipo de dados 
                                         -- corpos do reais, complexos
                                         -- o construtor todo eh o retorno

baskarar a b c
    | a == 0 = None
    | delta < 0 = Complex
    | otherwise = Roots x1 x2
    where
        delta = b*b-4*a*c
        x1 = (-b + sqrt(delta)) / (2*a)
        x2 = (-b - sqrt(delta)) / (2*a)

--typeclass

--baskar a b c
  --  | a == 0 = "polinômio inválido"
  --  | delta < 0 = "raízes complexas" 
   -- | otherwise = Roots x1 x2 -- = "raízes: " ++ (show x1 show x2)E
   -- where
    --    delta = b*b-4*a*c
    --    x1 = (-b + sqrt(delta)) / (2*a)
     --   x2 = (-b - sqrt(delta)) / (2*a)


data Pessoa = Pessoa {
        nome :: String,
        idade :: Int,
        salario :: Float
    }

rget'nome :: Pessoa -> String
rget'nome (Pessoa n _ _) = n


rget'nome (Pessoa n _ _) = n


