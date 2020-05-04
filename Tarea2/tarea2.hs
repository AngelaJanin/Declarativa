-- Integrantes del equipo:
-- Ángeles Martínez Ángela Janín
-- Rebollar Pérez Ailyn

import Data.List 

-- Ejercicio 1 
-- Función que encuentra la potencia de 2 más grande menor a n
gtrPower2 :: Int -> Int
gtrPower2 n = lista n n 
-- Función auxiliar que busca la potencia de 2 más grande menor a m a partir de la lista 
-- de potencias del número x.
lista :: Int -> Int -> Int
lista m x = if m <= last [2^n | n <- [0.. x]] then lista m (x-1) else last [2^n | n <- [0..x]]

-- Ejercicio 2
-- Función que regresa el mayor número de apariciones consecutivas del mismo elemento
inarow :: Eq a => [a] -> Int
inarow xs = maximum (map length (group xs))