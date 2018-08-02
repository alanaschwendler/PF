-- Exercicios para P2
--

data Tree a = Leaf a | Node a (Tree a) (Tree a) --pode ser uma folha ou um nodo com duas folhas
 deriving(Eq, Show)

tree1 :: Tree Int
tree1 = Node 8 (Node 2 (Leaf 2) (Leaf 4)) (Leaf 7)

mult2 :: Tree Int -> Int
mult2 (Leaf l) =  l * 2
mult2 (Node l t1 t2) = (l * 2) + mult2 t1 + mult2 t2

count :: Tree Int -> Int
count (Leaf l) = 1
count (Node l t1 t2) = 1 + count t1 + count t2

biggest :: Tree Int -> Int 
biggest (Leaf l) = l
biggest (Node l t1 t2) 
 |l > biggest t1 && l > biggest t2 = l
 |biggest t1 > biggest t2 = biggest t1
 |otherwise =  biggest t2

find :: Tree Int -> Int -> Bool
find (Leaf l) n  = l == n
find (Node l t1 t2) n 
 | l == n = True
 | otherwise = (find t1 n) || (find t2 n)

findCount :: Tree Int -> Int -> Int
findCount (Leaf l) n = if (n == l) then 1 else 0
findCount (Node l t1 t2) n
 | l == n = 1 + (findCount t1 n) + (findCount t2 n)
 | otherwise = (findCount t1 n) + (findCount t2 n)

removeSpaces :: [Char] -> [Char]
removeSpaces [] = []
removeSpaces xs = [x | x <- xs, x /= ' ']


elemento :: Eq a => a -> [a] -> Bool
elemento n [] = False
elemento n (x:xs) = n == x || elemento n xs

element :: Int -> [Int] -> Bool
element n [] = False
element n (x:xs)
 | matches n (x:xs) /= [] = True
 | otherwise = element n xs

multList :: Int -> [Int] -> [Int]
multList n [] = []
multList n (x:xs) = (n * x):(multList n xs)

matches :: Int -> [Int] -> [Int] 
matches n [] = []
matches n (x:xs)
 | n == x = n:(matches n xs)
 | otherwise = matches n xs

compose :: Int -> [Int] -> [Int]
compose n [] = []
compose n (x:xs) = ((multList n) . (matches n)) (x:xs)
