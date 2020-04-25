-- Integrantes del equipo:
-- Ángeles Martínez Ángela Janín
-- Rebollar Pérez Ailyn
module Reposicion where
    import Data.List 
    import Data.Maybe
    import Data.Bool
    import Data.Char

    -- Primer ejercicio Tarea 2
    todos :: Eq a => (a -> Bool) -> [a] -> Bool
    todos _ [] = True
    todos l m = if length (filter(l) m) == length m then True else False

    alguno :: Eq a => (a -> Bool) -> [a] -> Bool
    alguno _ [] = True
    alguno l m = if (filter (l) m) == [] then False else True
    
    toma :: (a -> Bool) -> [a] -> [a]
    toma l m = [y | (x,y) <- (comparaToma r)] 
        where r = buildTuples (map (l) m) m   

    deja :: (a -> Bool) -> [a] -> [a]
    deja l m = [y | (x,y) <- (comparaDeja r)] 
        where r = buildTuples (map (l) m) m       

    -- Funciones auxiliares
    comparaToma :: [(Bool,a)] -> [(Bool,a)]       
    comparaToma [] = []
    comparaToma (x:xs) = if fst x == True then x : comparaToma xs else []

    comparaDeja :: [(Bool,a)] -> [(Bool,a)]   
    comparaDeja [] = []    
    comparaDeja (x:xs) = if fst x == True then comparaDeja xs  else  x : xs 
    
    buildTuples :: [a] -> [b] -> [(a,b)]
    buildTuples [] [] = []
    buildTuples (x:xs) (y:ys) = (x,y) : buildTuples xs ys 

    -- Segundo ejercicio Tarea 2
    altMap :: (a -> b)-> (a -> b)-> [a] -> [b]
    altMap m n [] = [] 
    altMap m n [o] = [m o]
    altMap m n [o, p] = [m o, n p] 
    altMap m n (x:y:ys) = m x : n y : altMap m n ys

    --Tercer ejercicio Tarea 2
    luhn :: [Int] -> Bool
    luhn m = if (r `mod` 10) == 0 then True else False
        where r = foldr (+) 0 [if (x > 9) then (x - 9) else x | x <- altMap (*2) (+0) m]

    --TAREA 3 
    --Primer ejercicio 

    factorion :: Int -> Int 
    factorion l = foldr (+) 0 $ map factorial $ num2list l

    factorial :: Int -> Int
    factorial 0 = 1
    factorial n = n * factorial (n - 1)    

    num2list :: Int -> [Int]
    num2list m = map fromIntegral $ [toInteger (digitToInt x) | x <- show m]

    --Segundo ejercicio 
    iflip :: Int -> Int 
    iflip l = list2Num $ foldl (\m x-> x : m) [] (num2list l)
        
    list2Num :: [Int] -> Int
    list2Num = read . concatMap show

    --Tercer ejercicio 
    --binarios :: [Int] -> [Int]


        








