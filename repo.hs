module Reposicion where
    import Data.List 
    import Data.Maybe
    import Data.Bool

    todos :: Eq a => (a -> Bool) -> [a] -> Bool
    todos _ [] = True
    todos l m = if length (filter(l) m) == length m then True else False

    alguno :: Eq a => (a -> Bool) -> [a] -> Bool
    alguno _ [] = True
    alguno l m 
        | r == [] = False
        | otherwise = True
        where r = filter (l) m




