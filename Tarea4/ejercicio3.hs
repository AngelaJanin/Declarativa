-- Integrantes del equipo:
-- Ángeles Martínez Ángela Janín
-- Rebollar Pérez Ailyn

import Data.List

-- Ejercicio 3
-- Definimos las acciones
type Action = (Int,Int)

cantaros :: Int -> Int -> Int -> [Action]
cantaros x y z 
        | x == z = [(0,0), (x,0)]
        | y == z = [(0,0), (0,y)]
        | (z > x && z > y) = error "No se puede obtener el valor exacto en un cántaro"
        | x > y = creaAcciones [(0,0), (llenaR (0,0) x "l")] x y z
        | otherwise = creaAcciones [(0,0), (llenaR (0,0) y "r")] x y z

-- Llena el cántaro con el río completamente
llenaR :: Action -> Int -> String -> Action
llenaR a v c 
        | c == "r" = ((fst a), v)
        | otherwise = (v, (snd a))

-- Llena el cántaro con otro cántaro
llenaC :: Action -> String -> Int -> Action
llenaC a c v
        | c == "l2r" = (((fst a) - v), v)
        | otherwise = (v, (snd a) - v)

-- Vacia alguno de los cántaros o ambos al río
vacia :: Action -> String -> Action
vacia a c 
        | c == "l" = (0, (snd a))
        | c == "r" = ((fst a), 0)
        | otherwise = (0,0)

-- Función auxiliar que dada la lista de acciones que recibe
-- revisa el último estado tratando de llenar un cántaro con otro
creaAcciones :: [Action] -> Int -> Int -> Int -> [Action]
creaAcciones a x y z 
        | l == z || r == z = a
        | l > z && r == 0 = creaAcciones (a ++ [(llenaC (last a) "l2r" y)]) x y z  
        | r > z && l == 0 = creaAcciones (a ++ [(llenaC (last a) "r2l" x)]) x y z
        | x `mod` y /= 1 = error "No se puede obtener el valor exacto en un cántaro"
        where acc = (last a)
              l = (fst acc)
              r = (snd acc)