module Combinatory where

traverseList :: ([a] -> a -> [a] -> [b]) -> [a] -> [b]
traverseList f []     = []
traverseList f (x:xs) = traverseList' f [] x xs


traverseList' :: ([a] -> a -> [a] -> [b]) -> [a] -> a -> [a] -> [b] 
traverseList' f ini x [] = f ini x []
traverseList' f ini x xs = (f ini x xs) ++ (traverseList' f (ini ++ [x]) (head xs) (tail xs))   


getAllPermutations :: [a] -> [[a]]
getAllPermutations l = getAllVariations (length l) l 


getAllVariations :: Int -> [a] -> [[a]]
getAllVariations 0 _       = [[]]
getAllVariations length [] = [[]]
getAllVariations length l  = traverseList (\ini act tai -> map (act :) (getAllVariations (length - 1) (ini ++ tai))) l


getAllVariationsWithRepeate :: Int -> [a] -> [[a]]
getAllVariationsWithRepeate 0       _ = [[]]
getAllVariationsWithRepeate length [] = [[]]
getAllVariationsWithRepeate length l  = concat $ map (\x -> map (x:) (getAllVariationsWithRepeate (length - 1) l)) l

getAllPermutationsWithRepeate l = getAllVariationsWithRepeate (length l) l