module Twentysix where

import BinarySearchTree
import Control.Monad.Trans.State

type Remainder = Integer
type Position  = Integer

data RemPosPair = RPP { remainder :: Remainder 
                      , position  :: Position }

instance Eq RemPosPair where
 (==) a b = (==) (remainder a) (remainder b)

instance Ord RemPosPair where
 compare a b = compare (remainder a) (remainder b) 

data RemainderState = RSt { remainderSet  :: ScapeGoatBinaryTree RemPosPair
                          , lastRemainder :: RemPosPair}

type ReciprocalComputationState = State RemainderState

numToRecurringCycleLength :: Integer -> Integer
numToRecurringCycleLength num = evalState (searchForEndOfCycle num) (RSt emptySGTree (RPP 1 0))

getResultTree (Balanced tree) = tree
getResultTree (ISGT tree _)   = tree  

searchForEndOfCycle num =
 do
  RSt (SG bf nc mnc bt) (RPP prevRem prevPosition) <- get
  newRem                                           <- return $ rem (prevRem * 10) num 
  if (newRem == 0) 
  then return $ prevPosition - firstPosition + 1
  else
   case (insertIntoSGBST bf (RPP newRem (prevPosition + 1)) bt) of
    (NoChange (RPP _ firstPos)) -> return $ prevPosition - firstPos + 1
    (insertResult)              -> do
     put $ RSt (SG bf (nc + 1) (nc + 1) (getResultTree insertResult)) (RPP newRem (prevPosition + 1)) 
     searchForEndOfCycle num 

solver cap = maximum (map numToRecurringCycleLength [1 .. cap])
   

  
  
