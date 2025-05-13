funcao n =
    if n < 0
    then
        "Negativo"
    else
        "Positivo ou negativo"

soma x y = x + y

splitGui (a:xs) n = (take n (a:xs), drop n (a:xs))

--funcao zip soh vai ateh o tamanho da menor

dobroDez n = maiorDez(n*2)

maiorDez x = x > 10

swap i j ls = take i ls ++ [ls !! j] ++ take (j-i-1) (drop (i+1) ls) ++ [ls !! i] ++ drop (j+1) ls
    -- comeco
    -- pega do inicio antes do primeiro indice
    -- meio
    -- fim
    -- primer

dupla d = "Ola, " ++ (fst d) ++ "! Voce tem " ++ show (snd d) ++ " anos."

distance ()
