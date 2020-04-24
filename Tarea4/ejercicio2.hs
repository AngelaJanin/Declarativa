-- Integrantes del equipo:
-- Ángeles Martínez Ángela Janín
-- Rebollar Pérez Ailyn
module Patos where 
    import Data.List
    import Data.Char
    import Data.Maybe

    type Coord = (Int, Int)
    type Pato = (Coord, Char)
    type Generation = [Pato]

    --Función que construye una generación inicial a partir 
    --de una cadena de entrada con el siguiente formato 
    genZero :: String -> Generation
    genZero m = construyePatos (patosBoard (altoBoard m) (anchoBoard m)) (preStr(m)) 

    --Función que calcula la siguiente generación de patos evolucionando el tablero 
    evolution :: Generation -> Generation
    evolution [] = [] 
    evolution m = map (conviertePato m) m

    --Función que recibe una generación inicial y calcula el ciclo de evoluciones 
    --de esa generación hasta que ya no haya cambios con respecto a la generación anterior 
    generations :: Generation -> [Generation]
    generations [] = []
    generations m
        | m == n = n : []
        | otherwise = m : generations (n)
            where n = evolution m   

    --Función auxiliar que a partir de la cadena de entrada calcula el alto del tablero
    altoBoard :: String -> Int
    altoBoard str = (length $ filter (== '_') str) + 1

    --Función auxiliar que a partir de la cadena de entrada calcula el ancho del tablero
    anchoBoard :: String -> Int
    anchoBoard str = length (subStr ['_'] str) 

    --Función auxiliar que corta la cadena hasta encontrar cierto caracter 
    subStr :: String -> String -> String                           
    subStr xs [] = [] 
    subStr [] ys = [] 
    subStr xs (y:ys) = if isPrefixOf xs (y:ys)
                      then []
                      else y:(subStr xs (tail (y:ys)))    

    --Función auxiliar que elimina los guiones bajos de la cadena de entrada
    preStr :: String -> String 
    preStr xs = [x| x <- xs, not(x `elem` "_")]

    --Función auxiliar que calcula todas las coordenadas posibles del tablero según
    --las medidas ancho y alto
    patosBoard :: Int -> Int -> [Coord]
    patosBoard m n = [(x,y)| x<-[0..m-1], y<-[0..n-1]]

    --Función auxiliar que construye Patos con su maldad (B o G) y su posición en el
    --tablero
    construyePatos :: [a] -> [b] -> [(a,b)]
    construyePatos [] [] = []
    construyePatos [] _ = []
    construyePatos _ [] = []
    construyePatos (x:xs) (y:ys) = (x,y) : construyePatos xs ys

    --Función auxiliar que obtiene todas las posiciones vecinas dado un pato
    obtenVecindad :: Pato -> [Coord]
    obtenVecindad ((x,y),m) = [(x-1, y-1),(x, y-1),(x+1, y-1),(x-1, y),(x+1, y),(x-1, y+1),(x, y+1),(x+1, y+1)]

    --Función auxiliar que obtiene la cantidad de patos buenos en la vecindad de un pato
    buenosEnVecindad :: Generation -> Pato -> Int
    buenosEnVecindad a b = cuentaBuenos $ map fromJust $ filter (/= Nothing) $ map (\m -> lookup m a) $ obtenVecindad b 

    --Función auxiliar que cuenta la cantidad de patos buenos dada una cadena
    cuentaBuenos :: String -> Int
    cuentaBuenos str = length $ filter (== 'G') str

    --Función auxiliar que convierte al pato aplicando las reglas del ejercicio
    conviertePato :: Generation -> Pato -> Pato
    conviertePato g ((x,y),z) 
        | z == 'G' && (buenosEnVecindad g ((x,y),z) == 2 || buenosEnVecindad g ((x,y),z) == 3) = ((x,y),z)
        | z == 'B' && buenosEnVecindad g ((x,y),z) == 3 = ((x,y),'G')
        | otherwise = ((x,y),'B')    

    

