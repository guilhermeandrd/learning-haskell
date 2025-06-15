data Pessoa = Pessoa {
    nome  :: String, 
    idade :: Int 
} deriving Show

saudacao :: Pessoa -> String
saudacao p = "Olá " ++ nome p ++ "! Você tem " ++ show (idade p) ++ " anos!"
