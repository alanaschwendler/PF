vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 3
vendas 2 = 2
vendas 3 = 4
vendas 4 = 2
vendas n = n * 2

vendaTotal :: Int -> Int
vendaTotal n 
 | n == 0    = vendas 0
 | otherwise = vendas n + vendaTotal (n-1)

tabela :: Int -> String
tabela n = cabecalho ++ geraVendas n ++ total n

cabecalho :: String
cabecalho = "\nSemana      Vendas\n"

geraString :: Int -> String
geraString n = "\nSemana " ++ show n ++ "       " ++ show (vendas n)

geraVendas :: Int -> String
geraVendas 0 = geraString 0
geraVendas n = geraVendas(n-1) ++ geraString n 

total :: Int -> String 
total n = "\nTotal:         " ++ show (vendaTotal n) ++ "\n\n"
-----------------------------------------------------------------
-- tipos algebricos

data Temperatura = Frio | Quente
 deriving(Show, Eq)

data Estacao = Verao | Outono | Inverno | Primavera
 deriving(Show, Eq)

tempo :: Estacao -> Temperatura
tempo Verao = Quente 
tempo Primavera = Quente
tempo _ = Frio
-----------------------------------------------------------------
-- listas

somaPares :: [(Int, Int)] -> Int 
somaPares [] = 0
somaPares (x:xs) = c + d + somaPares xs
 where -- x é uma tupla
 c = fst x
 d = snd x

 -- OU
 
somaPares2 :: [(Int, Int)] -> Int 
somaPares2 [] = 0
somaPares2 ((c, d):xs) = c + d + somaPares2 xs
