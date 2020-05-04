import Data.List
import Data.Ord

-- Función aux que devuelve la longitud de una lista entre 2
longitud ::[a] -> Int
longitud xs = div (length xs) 2

-- Función aux que toma una lista y la divide en 2 tuplas según un índice dado
parte::[a] -> ([a],[a])
parte xs = splitAt (longitud xs) xs

-- MergeSort 
mezcla::(Ord a) => [a] -> [a] -> [a]
mezcla [] [] = []
mezcla [x] [] = [x]
mezcla [] xs = xs
mezcla xs ys  
    | x <= y = [x] ++ mezcla (tail xs) ys
    | x >= y = [y] ++ mezcla xs (tail ys)
    where x = head xs
          y = head ys

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mezcla (mergeSort f) (mergeSort s)
    where (f, s) = parte xs 

--MergeSortCon
mezclaCon :: (Ord a)  => (a -> a -> Ordering) -> [a] -> [a] -> [a]
mezclaCon _ [] [] = []
mezclaCon _ [x] [] = [x]
mezclaCon _ [] xs = xs 
mezclaCon compara xs ys 
    | compara x y == LT = [x] ++ mezclaCon compara (tail xs) ys
    | otherwise = [y] ++ mezclaCon compara xs (tail ys)
        where x = head xs
              y = head ys

mergeSortCon :: (Ord a)  => (a -> a -> Ordering) -> [a] -> [a]
mergeSortCon _ [] = []
mergeSortCon _ [x] = [x]
mergeSortCon compara xs = mezclaCon compara (mergeSortCon compara n) (mergeSortCon compara l)
    where (n, l) = parte xs

