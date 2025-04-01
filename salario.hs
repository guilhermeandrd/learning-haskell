
salario :: Integer -> Integer

salario x | x >= 1 && x <= 10 = 100
          | x > 10 && x < 21 = 200
          | x > 20 && x < 31 = 300
          | x > 30 && x < 41 = 400
          | x > 40 = 500