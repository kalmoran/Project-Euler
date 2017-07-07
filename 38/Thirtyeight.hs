module Thirtyeight where

import Data.List
import Data.Maybe
import DigitOperations

relativePrimes = [1, 2, 3, 5, 7, 11, 13, 17, 19, 23]
targetProd     = product relativePrimes 

generalSolver interval cof = filter isJust $ map (inspectNum . (* cof)) interval

inspectNum num = 
 case (targetProd == (product . (map (\x -> relativePrimes !! x)) . numToDigitList) num) of
  (True)  -> Just num
  (False) -> Nothing  

nSolver 2 = generalSolver [5000 .. 9999] 100002  
nSolver 3 = generalSolver [100 .. 333] 1002003
nSolver 4 = generalSolver [25 .. 33] 10203004
nSolver 5 = generalSolver [5 .. 9] 102030405
nSolver 6 = generalSolver [3] 123040506
--nSolver 7 = []
--nSolver 8 = []
--nSolver 9 = generalSolver [1] 123456789

solver = maximum $ map (fromJust) $ concat $ map nSolver [2 .. 6] 

 