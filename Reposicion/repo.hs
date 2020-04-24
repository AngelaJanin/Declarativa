module Reposicion where
    import Data.List 
    import Data.Maybe
    import Data.Bool

    todos :: Eq a => (a -> Bool) -> [a] -> Bool
    todos _ [] = True
    todos l m 
        | r == [] = True
        | otherwise = False
        where r = filter (l) m
    
    --alguno :: Eq a => (a -> Bool) -> [a] -> Bool



