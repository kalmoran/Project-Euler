module TwentyOne where

-- dev note: for higher caps (=> 1000000) we need a faster method for prime divisor search

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Data.Array.IO
import Data.List

type Prime   = Integer
type Divisor = Integer

data PrimitiveDivisionData = Uncalculated | Pr | PDD [Divisor]

type Array = IOArray Integer PrimitiveDivisionData

type CalculationState = StateT Array IO

newStateArray :: Integer -> IO Array 
newStateArray cap = do 
 arr <- newArray (1, cap) Uncalculated
 writeArray arr 1 Pr -- 1 is not a prime in theory, we can treat it as a prime here
 writeArray arr 2 Pr -- findFirstPrimeDivisors doesn't work with 2
 return arr

calculateNewDivisors pr divs = union divs (map ( * pr) divs) -- provided divs contains 1!

findFirstPrimeDivisor :: Integer -> Maybe (Prime, Integer)
findFirstPrimeDivisor 2      = error "findFirstPrimeDivisor: cannot be called with 2"
findFirstPrimeDivisor number = findFirstPrimeDivisor' (ceiling (sqrt (fromIntegral number)))  number 2 where
 findFirstPrimeDivisor' c n d = 
  if (d > c) 
  then Nothing
  else 
   let (d2, m) = quotRem n d in
    if (m == 0) 
    then Just (d, d2)
    else findFirstPrimeDivisor' c n (d + 1)

writeDivisors :: Integer -> PrimitiveDivisionData -> CalculationState ()
writeDivisors n pd = get >>= \arr -> liftIO $ writeArray arr n pd 

calculateProperDivisors :: Integer -> CalculationState [Divisor]
calculateProperDivisors n = do
 arr <- get
 el  <- liftIO $ readArray arr n
 case (el) of
  (Uncalculated) -> 
   case (findFirstPrimeDivisor n) of
    (Nothing)       -> do
     writeDivisors n Pr
     return [1]
    (Just (pd, d2)) -> do
     restOfDivisors <- calculateProperDivisors d2
     divisors       <- return $ union [d2] $ calculateNewDivisors pd restOfDivisors
     writeDivisors n (PDD divisors)
     return divisors
  (Pr)     -> return [1]
  (PDD ds) -> return ds

processNumber :: Integer -> Integer -> CalculationState Integer
processNumber cap n = do
 divisors      <- calculateProperDivisors n
 pairCandidate <- return $ sum divisors
 if (pairCandidate >= n && pairCandidate <= cap) 
 then do
  pairCandidateDivSum <- calculateProperDivisors pairCandidate >>= (return . sum)
  if (pairCandidateDivSum == n)
  then return (n + pairCandidate)
  else return 0
 else return 0

collectAmicableNumbersSum :: Integer -> CalculationState Integer
collectAmicableNumbersSum cap = liftM sum $ mapM (processNumber cap) [3 .. cap] 
 
solver :: Integer -> IO Integer
solver cap = newStateArray cap >>= evalStateT (collectAmicableNumbersSum cap)

solver2 cap = newStateArray cap >>= evalStateT (mapM (processNumber cap) [3 .. cap]) >>= return . (filter (not . (==0)))  


 




