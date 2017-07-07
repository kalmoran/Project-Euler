module Forty where

import DigitOperations

champernowneList = concat $ map numToDigitList [1 ..]

solver = 
   champernowneList !! (1 - 1)
 * champernowneList !! (10 - 1)
 * champernowneList !! (100 - 1)
 * champernowneList !! (1000 - 1)
 * champernowneList !! (10000 - 1)
 * champernowneList !! (100000 - 1)
 * champernowneList !! (1000000 - 1)