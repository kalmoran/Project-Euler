module Fortyfour where

import Control.Monad
import Control.Monad.Trans.State
import Data.Maybe

type PentagonalNumber = Integer
type PentagonalCount  = Integer

data PentagonalValueNPair = PVNP { value :: PentagonalNumber 
                                 , count :: PentagonalCount 
                                 } deriving (Show)

instance Eq PentagonalValueNPair where
 (==) a b = (value a) == (value b)
 
instance Ord PentagonalValueNPair where
 compare a b = compare (value a) (value b)

generateNthPentagonalNumber n = div (n * (3 * n - 1)) 2

generatePentagonalNumbersFromNToCap startingN cap = takeWhile (<= (PVNP cap 0)) $ map (\(a, b) -> PVNP a b) $ zip (map generateNthPentagonalNumber [startingN..]) [startingN]

type Diff = Integer

data PentagonalState = PS { currentCount            :: PentagonalCount
                          , diffList                :: [(Diff, PentagonalCount)]
                          , currentMinimumDif       :: Diff 
                          }

intToFloat :: Integer -> Float
intToFloat = fromIntegral

isPentagonalNumber :: Integer -> Maybe ()
isPentagonalNumber pn = 
 let dis = sqrt (intToFloat (1 + 24 * pn)) in
  let disInt = floor dis in
   if (dis == intToFloat (disInt)) && (mod (1 + disInt) 6 == 0)
   then Just ()
   else Nothing 


isSumPentagonal otherV (PVNP dif c) = isPentagonalNumber (generateNthPentagonalNumber c + otherV) >> Just dif
isDiffPentagonal (diff, c)          = isPentagonalNumber diff >> Just (PVNP diff c) 
   
solver = currentMinimumDif $ execState solverCore (PS 1 [] (-1))    

solverCore = do
 (PS currentC diffL currentMDiff) <- get
 currentNum                       <- return $ generateNthPentagonalNumber currentC
 nextNum                          <- return $ generateNthPentagonalNumber (currentC + 1)
 newDiff                          <- return $ nextNum - currentNum
 when ((currentMDiff == (-1) || newDiff < currentMDiff)) (do
  newDiffList <- return $ (newDiff, currentC) : (map (\(d, c) -> (d + newDiff, c)) diffL)
  case (catMaybes ((isDiffPentagonal >=> isSumPentagonal nextNum) <$> newDiffList)) of
   (d:_) -> when (d < currentMDiff || currentMDiff == (-1)) (modify (\(PS c dl _) -> PS c dl d))
   (_)   -> return ()
  modify (\(PS c dl md) -> PS (c + 1) newDiffList md)
  solverCore
  )
 
