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
    alguno l m = if (filter (l) m) == [] then False else True
        
    toma :: (a -> Bool) -> [a] -> [a]
    toma l = filter l 
    
    deja :: (a -> Bool) -> [a] -> [a]
    deja l = filter $ not . l




