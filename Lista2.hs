import Data.Char
--Lista 2 - Programacao Funcional
--Ana Elisa Ghanem Zanon

--Ex1
pertence e [ ] = False
pertence e (x:xs) = if x == e then True else pertence e xs

--Ex2
intercessao [ ] ys = [ ]
intercessao (x:xs) ys = if pertence x ys then x:intercessao xs ys else intercessao xs ys

--Ex3
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

--Ex4
nprimeiros _ []  = []
nprimeiros 0 _ = []
nprimeiros n (x:xs) = x:nprimeiros (n-1) xs
nUltimos n (x:xs) = inverso(nprimeiros n (inverso xs))

--Ex5
soma2 _ [] = []
soma2 [] _ = []
soma2 (x:xs) (y:ys) = (x + y):soma2 xs ys

--Ex6
pot2 n = inverso(pot2' n)
pot2' 0 = []
pot2' n = 2 ^ n:pot2' (n-1)

--Ex7
intercalacao [] ys = ys
intercalacao xs [] = xs
intercalacao (x:xs) (y:ys) | y < x = y:intercalacao ys (x:xs)
                           | y > x = x:intercalacao (y:ys) xs
                           |otherwise = x:intercalacao (y:ys) xs
   
--Ex8
menor [x] = x
menor (x:xs) | x < menor xs = x
             |otherwise = menor xs

--Ex9
removerElem a [] = []
removerElem a (x:xs) |a == x = xs
                     |otherwise = x:removerElem a xs

--Ex10
ordenar [] = []
ordenar (x:xs) = menor (x:xs): ordenar (removerElem (menor (x:xs)) (x:xs))

--Ex11
ins n [] = [n]
ins n (x:xs) |n == x = (x:xs)
             |n < x = n:(x:xs)
             |otherwise = x:ins n xs

--Ex12
enesimo _ [] = error "Não existe nenhum elemento nessa posição"
enesimo n (x:xs) |n==1 = x
                  |otherwise = enesimo (n-1) xs

--Ex13
repetir 0 e = []
repetir n e = e:repetir (n-1) e

--Ex14
numString 0 = []
numString n = numString (div n 10) ++ [(chr ((rem n 10) + 48))]

--Ex15
stringNum [] = 0
stringNum (x:xs) = (((ord x) -48) * 10^length xs) + stringNum xs

--Ex16
bin2int [] = 0
bin2int (x:xs) = (((ord x) -48) * 2^length xs) + bin2int xs

--Ex17
int2bin 0 = []
int2bin n = int2bin (div n 2) ++ [(chr ((rem n 2) + 48))]

--Ex18
minusculas :: [Char] -> [Char]
minusculas [] = []
minusculas (x:xs) = if elem x ['a'..'z']  then x:minusculas xs
                    else if elem x ['A'..'Z'] then toEnum((fromEnum x) + 32):minusculas xs
                    else error "Lista contém um caracter que não é uma letra"
