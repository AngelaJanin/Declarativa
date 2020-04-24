-- Integrantes del equipo:
-- Ángeles Martínez Ángela Janín
-- Rebollar Pérez Ailyn

import Data.List
import Data.Char
import Data.Maybe
-- Lista auxiliar con los dígitos del 0 al 9
digitos :: [Int]
digitos = [0..9]
-- Lista auxiliar con los operadores válidos
operadores :: [Char]
operadores = ['+', '-', '*', '=']

-- Ejercicio 1
-- Función que regresa todas las posibles soluciones del criptoaritmo
criptoaritmos :: String -> [[(Char,Int)]]
criptoaritmos c = filter (checaEc (words c)) (asignaciones c)

-- Función que obtiene las letras sin espacios y sin ser repetidas
obtenLetras :: String -> String
obtenLetras xs = nub (filter (\x -> (not (isSpace x)) && notElem x operadores)  xs)

-- Función que obtiene las iniciales de las palabras de la ecuación
obtenIniciales :: String -> String
obtenIniciales xs = nub (map (\y -> y!!0) (words (filter (\x -> notElem x operadores) xs)))

-- Función auxiliar que asigna un dígito a una letra
asignaDigito :: String -> [Int] -> [(Char, Int)]
asignaDigito [] nums = []
asignaDigito palabra [] = []  
asignaDigito (x:xs)(y:ys) = [(x, y)] ++ (asignaDigito xs ys)

-- Función auxiliar que checa que una asignación a una letra inicial no sea 0
checaAssig :: [(Char,Int)] -> String -> Bool
checaAssig [] c = True
checaAssig (x:xs) c
        | elem (fst x) (obtenIniciales c) && (snd x) == 0 = False
        | otherwise = checaAssig xs c

-- Función auxiliar que asigna dígitos a todas las letras de todas las palabras
-- y checa que las letras iniciales no tengan asignadas el número 0
asignaDigitos :: String -> [Int] -> [[(Char, Int)]]            
asignaDigitos xs nums  = filter (`checaAssig` xs) (map (`asignaDigito` nums) (permutations xs))

-- Función auxiliar que asigna dígitos a todas las letras con todas las posibles
-- combinaciones de dígitos
asignaciones :: String -> [[(Char,Int)]]
asignaciones c = concatMap (asignaDigitos (obtenLetras c)) (permutations digitos)

-- Función auxiliar que dada la palabra y una lista con las asignaciones
-- pasa la palabra a una lista de dígitos
numeros :: String -> [(Char, Int)] -> [String]
numeros p xs = (map (show . fromJust) (map (`lookup` xs) p))

-- Función auxiliar que junta a todos los dígitos en un número
formaNum :: [String] -> String
formaNum (x:xs) 
    | null xs = x
    | otherwise = x ++ formaNum xs

-- Función auxiliar que transforma un número en cadena a un número entero
entero :: String -> Int
entero num = read num::Int 

-- Función auxiliar que checa que se cumpla la ecuación dada la lista de palabras y operadores
-- junto con la lista de asignaciones
checaEc :: [String] -> [(Char, Int)] -> Bool
checaEc p asig
    | ((p!!1) == "+") && (n1 + n2 == n3) = True
    | ((p!!1) == "-") && (n1 - n2 == n3) = True
    | ((p!!1) == "*") && (n1 * n2 == n3) = True  
    | otherwise = False
    where n1 = (entero (formaNum (numeros (p!!0) asig)))
          n2 = (entero (formaNum (numeros (p!!2) asig)))
          n3 = (entero (formaNum (numeros (p!!4) asig)))