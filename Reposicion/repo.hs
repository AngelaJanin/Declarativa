module Reposicion where
    import Data.List 
    import Data.Maybe
    import Data.Bool
    import Data.Char

    todos :: Eq a => (a -> Bool) -> [a] -> Bool
    todos _ [] = True
    todos l m = if length (filter(l) m) == length m then True else False

    alguno :: Eq a => (a -> Bool) -> [a] -> Bool
    alguno _ [] = True
    alguno l m 
        | r == [] = False
        | otherwise = True
        where r = filter (l) m

    toma :: (a -> Bool) -> [a] -> [a]
    toma _ [] = []
    toma l m = filter l m

    deja :: (a -> Bool) -> [a] -> [a]
    deja _ [] = []
    deja l m = notFilter l m 

    --Función auxiliar para la función deja

    notFilter :: (a -> Bool) -> [a] -> [a]
    notFilter l = filter $ not . l




