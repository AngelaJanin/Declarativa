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
    toma l [] = []
    toma l (x:xs) = if l x then x : toma l xs else []
    
    deja :: (a -> Bool) -> [a] -> [a]
    deja l = filter $ not . l

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








