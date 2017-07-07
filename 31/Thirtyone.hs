module Thirtyone where

coins = [200, 100, 50, 20, 10, 5, 2, 1]

findAllPossibleCoinCombinations targetNum = recursiveSolver targetNum coins
 
 
recursiveSolver 0 _    = 1
recursiveSolver _  [1] = 1
recursiveSolver num [] = error "recursiveSolver called with empty coins list"
recursiveSolver num (x:xs) =
 let rest = recursiveSolver num xs in
  case (compare num x) of
   (LT) -> rest 
   (EQ) -> rest + 1
   (GT) -> rest + (recursiveSolver (num - x) (x:xs))