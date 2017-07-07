module Fifteen where

generateNextRowRecursive prevElem []     = []
generateNextRowRecursive prevElem (x:xs) = 
 let newElem = prevElem + x in
 newElem : (generateNextRowRecursive newElem xs)

generateNextRow [x]      = [x]
generateNextRow (x:xs) = generateNextRowRecursive (2 * x) xs 

recursiveSolver [x] = 2 * x
recursiveSolver xs  = recursiveSolver (generateNextRow xs)  

solver gridLength = recursiveSolver (replicate gridLength 1)
