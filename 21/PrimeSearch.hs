module TwentyOne where

import Data.Array.IO
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class

type Prime = Integer

data PrimitiveDivisionData = Uncalculated | Pr | PDD [Prime]

type Array = IOArray Integer PrimitiveDivisionData

type CalculationState = StateT Array IO

newStateArray :: Integer -> IO Array 
newStateArray cap = newArray (2, cap) Uncalculated

readStateArray :: Array -> Integer -> IO PrimitiveDivisionData
readStateArray = readArray 

mergePrimes :: [Prime] -> [Prime] -> [Prime] --both list sorted ascending
mergePrimes [] ys = ys
mergePrimes xs [] = xs
mergePrimes xs ys = 
 if (head xs < head ys)
 then head xs : mergePrimes (tail xs) ys
 else head ys : mergePrimes xs (tail ys) 

findFirstPrimeDivisor :: Integer -> Maybe (Prime, Integer)
findFirstPrimeDivisor number = findFirstPrimeDivisor' (ceiling (sqrt (fromIntegral number)))  number 2 where
 findFirstPrimeDivisor' c n d = 
  if (d > c) 
  then Nothing
  else 
   let (d2, m) = quotRem n d in
    if (m == 0) 
    then Just (d, d2)
    else findFirstPrimeDivisor' c n (d + 1)

writePrimeDivisors :: Integer -> PrimitiveDivisionData -> CalculationState ()
writePrimeDivisors n pd = get >>= \arr -> liftIO $ writeArray arr n pd 

calculatePrimeDivisors :: Integer -> CalculationState [Prime]
calculatePrimeDivisors n = do
 arr <- get
 el  <- liftIO $ readStateArray arr n
 case (el) of
  (Uncalculated) -> 
   case (findFirstPrimeDivisor n) of
    (Nothing)       -> do
     writePrimeDivisors n Pr
     return [n]
    (Just (pd, d2)) -> do
     restOfPrimeDivisors <- calculatePrimeDivisors d2
     primeDivisors       <- return $ mergePrimes [pd] restOfPrimeDivisors
     writePrimeDivisors n (PDD primeDivisors)
     return primeDivisors
  (Pr)          -> return [n]
  (PDD pds)     -> return pds


 




