import Data.List 

-- Función que elimina todas las mayúsculas de una cadena
quitaMayusculas :: String -> String
quitaMayusculas s = [lt | lt <- s, lt `notElem` ['A'..'Z']]

-- Función que elimina todos los caracteres que no sean
-- letras de una cadena
soloLetras :: String -> String 
soloLetras s = [x | x <- s, x `elem` ['A'..'Z'] `union` ['a'..'z'] ]

-- No se me ocurrió cómo hacerlo por listas de comprensión
-- :c

prefijo :: String -> String -> Bool
prefijo xs ys = elem xs (inits ys)  





