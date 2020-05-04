-- Composicion de funciones 
import Data.List

-- 
f :: [Int] -> [Int]
f xs = map (+ 10) (nub xs)

--
f2 :: [Int] -> Int
f2 xs = maximum (map (+ 10) (nub xs))

-- El pesito asocia a la derecha. Lo utilizas cuando ya tienes algo qué evaluar, un parámetro explícito 
f3 :: [Int] -> Int
f3 xs = maximum $ map (+ 10) $ nub xs 

-- Funciona como un alias. (Aprende Jasquel x el vien de to2)
f4 :: [Int] -> Int
f4 = maximum . map (+ 10) . nub 

-- maximun compuesta con map se aplia a la lista resultante de nub 
-- pesito porque le estoy explicitando la lista xs
f5 :: [Int] -> Int
f5 xs = (- 10) . maximum . map (+ 10) $ nub xs


