module Thirtysix where

import Control.Monad
import Control.Monad.Trans.State
import Data.List
import Data.Maybe
import Divisor



localGetDivisors :: IO (DivisionStateData ())
localGetDivisors = getDivisors primeDivisorUnion 999999


solver =
 getDivisors primeDivisorUnion 999999 
 >>= evalStateT (mapM (solverCore) [1 .. 999999]) >>= return . filter (/= [])
 -- >>= return . sum
 
solverCore :: Integer -> StateT (DivisionStateData ()) IO [Integer]
solverCore act = do 
 userData <- commonRead userArray act
 case userData of
  (Just _)  -> return []
  (Nothing) -> do
   circle          <- return $ getCircle act
   isCircularPrime <- (mapM isPrime circle >>= return . and)
   forM_ circle (\n -> writeUserData n ())
   if (isCircularPrime) 
    then return circle
    else return []


getCircle num = 
 let digitList = reverse (numToDigitList num) in
  getCircleRecursive digitList (length digitList) 


getCircleRecursive l 1 = [digitListToNum l]
getCircleRecursive l n = (digitListToNum l) : getCircleRecursive (tail l ++ [head l]) (n - 1)  