module Fortyone where

import Control.Monad
import Control.Monad.Trans.State
import DigitOperations
import Divisor

firstNinePrime = [1, 2, 3, 5, 7, 11, 13, 17, 19, 23]

firtNPrimeProd n = product $ take (n + 1) firstNinePrime -- n < 10 

solver = evalDivision (execStateT solverCore 987654321) lazyPrimeDivisionUnion (floor (sqrt (fromIntegral 987654321)))

isPanDigitalPrime n = do
 isPr <- isPrime n
 if (isPr) 
 then do
  digits    <- return $ numToDigitList n 
  numLength <- return $ length digits 
  return $ firtNPrimeProd numLength == product (map ((firstNinePrime !!) . fromIntegral) digits)
 else return False

solverCore = do
 actNum         <- get
 isPanDigitalPr <- isPanDigitalPrime actNum
 when (actNum == 0) (error "0")
 when (not (isPanDigitalPr)) (do
  put (actNum - 1)
  solverCore)