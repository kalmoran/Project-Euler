module Twentyfour where

import Data.List

fact 0 = 1
fact n = product [1 .. n]

findNthElementOfPermutaion :: (Ord a, Show a) => [a] -> Integer -> String 
findNthElementOfPermutaion elems n = 
 if (n > fact (toInteger (length elems))) 
 then error "n is greater than the number of possible permutations"
 else recursiveSolver (sort elems) (n - 1)

recursiveSolver :: (Ord a, Show a) => [a] -> Integer -> String
recursiveSolver elems 0 = concat $ map (show) elems
recursiveSolver elems n = 
 let currentStep = toInteger (length elems) in
  let (div, rem) = quotRem n (fact (currentStep - 1)) in
   let chosenElem = elems !! (fromIntegral div) in
    (show chosenElem) ++ recursiveSolver (delete chosenElem elems) rem
   
