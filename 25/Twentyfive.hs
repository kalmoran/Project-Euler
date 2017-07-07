module Twentyfive where

import Data.Array.IO
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans.State hiding (put, get, gets)

type Array = IOArray Integer Integer

type FibState = StateT (Integer, Array) IO

fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

getActFibNum :: FibState Integer
getActFibNum = do
 (act, arr) <- get
 liftIO $ readArray arr (mod act 2)

calculateNextFib :: FibState Integer
calculateNextFib = do
 (act, arr) <- get
 newFib     <- liftIO $ liftM2 (+) (readArray arr 0) (readArray arr 1)
 liftIO $ writeArray arr (mod (act + 1) 2) newFib
 put (act + 1, arr)
 return newFib

repeate grd action = do
 res    <- action
 isStop <- grd
 if (isStop) 
 then return res
 else repeate grd action

nthGuard n = do
 (act, _) <- get
 return (n == act)


digitNumGuard n = do
 actFib   <- getActFibNum
 digitNum <- return $ length $ show actFib
 return (n == digitNum)


calculateNthFib :: Integer -> IO Integer
calculateNthFib 1 = return 1
calculateNthFib 2 = return 1
calculateNthFib n = do
 arr <- newArray (0,1) 1
 evalStateT (repeate (nthGuard n) calculateNextFib) (2, arr)
 
indexOfFirstNDigitFib :: Int -> IO Integer
indexOfFirstNDigitFib 1      = return 1
indexOfFirstNDigitFib digNum = do
 arr    <- newArray (0,1) 1
 liftM fst $ execStateT (repeate (digitNumGuard digNum) calculateNextFib) (2, arr)
 


