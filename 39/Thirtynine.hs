module Thirtynine where

import Data.List

solver :: Int
solver = head $ maximumBy (\a b -> compare (length a) (length b)) $ group $ sort  $ concat $ map (solveWithA) [1 .. 499]

solveWithA :: Int -> [Int]
solveWithA a = 
 let bCap = div (1000 * (a - 500)) (a - 1000) in
  concat $ map (solveWithAB a) [a .. bCap]

solveWithAB :: Int -> Int -> [Int] 
solveWithAB a b = let c = floor (sqrt (fromIntegral (a^2 + b^2))) in
 if (a^2 + b^2 == c^2) 
 then [a + b + c]
 else []