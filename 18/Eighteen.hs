module Eighteen where

parseNumber :: String -> Integer
parseNumber = read

parseInput :: String -> [[Integer]]
parseInput = (map (map parseNumber) . map words . lines)  

transformAncestorRow x []     = [x]
transformAncestorRow x (y:ys) = (max x y) : (transformAncestorRow y ys)

mergeRows fr sr = map (\(x, y) -> x + y) $ zip (transformAncestorRow 0 fr) sr 

recursiveSolver [] (xs:xss) = recursiveSolver xs xss
recursiveSolver xs []       = xs
recursiveSolver xs (ys:yss) = recursiveSolver (mergeRows xs ys) yss

solver inputFilePath = do
 nums <- readFile inputFilePath >>= (return . parseInput)
 print (foldr (max) 0 (recursiveSolver [] (nums))) 
 







