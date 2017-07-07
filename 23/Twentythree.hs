module Twentythree where

import Control.Monad
import Control.Monad.Trans
import Data.Monoid
import Divisor

instance Monoid Integer where
 mempty  = 0
 mappend = (+)  

abundantInf = 12 -- the first abundant number
abundantSup = 28123 --all the number above this can be written as a sum of two abundant number 

newtype NumberAbundancy = Ab Integer 

type NonAbundantSum = Division NumberAbundancy

checkForAbundancy :: Integer -> NonAbundantSum ()
checkForAbundancy 1 = writeUserData 1 (Ab abundantInf)
checkForAbundancy n = do
 divData <- lift $ readDivisors n
 case (divData) of
  (Pr _)      -> return ()
  (DIVS divs) -> when ((sum divs) > n) (do
     (Just (Ab lastAbNum)) <- readUserData 1
     writeUserData 1 (Ab n)
     writeUserData lastAbNum (Ab n)
     writeUserData n (Ab (-1)))
  (_)         -> error "unprocessed divisors!"

abundantNumberIterator num (-1)           = return 0
abundantNumberIterator num actAbundantNum = do
 case (compare (actAbundantNum * 2) num) of 
  (GT) -> return num
  (EQ) -> return 0
  (LT) -> do
   (complementerAbundancy) <- readUserData (num - actAbundantNum)
   case (complementerAbundancy) of
    (Nothing) -> do
     (Just (Ab nextAbundantNum)) <- readUserData actAbundantNum
     abundantNumberIterator num nextAbundantNum
    (Just (Ab _))  -> return 0  

solver = iterateOnInterval (\n -> checkForAbundancy n >> (abundantNumberIterator n abundantInf)) properDivisorUnion abundantSup
 

   

  



