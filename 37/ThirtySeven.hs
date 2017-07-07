module ThirtySeven where

import Data.List
import DigitOperations
import Divisor
import Control.Monad
import Control.Monad.Trans.State hiding (put, get, gets, modify)
import Control.Monad.Trans.Writer hiding (tell)
import Control.Monad.State.Class
import Control.Monad.Writer.Class

type Solver = StateT (Int, Integer) (WriterT [Integer] Division)

isTruncatablePrime :: MonadDivisor m => Integer -> m Bool
isTruncatablePrime n = do
 digList <- return $ numToDigitList n
 case (findIndex (\x -> mod x 2 == 0 && x /= 2) digList) of
  (Just _)  -> return False
  (Nothing) -> do
   prefs   <- return $ map ((flip take) digList) [1 .. (length digList)]
   sufs    <- return $ map ((flip drop) digList) [1 .. ((length digList) - 1)]
   mapM (isPrime . digitListToNum) (prefs ++ sufs) >>= return . and
  
solver :: IO [Integer]
solver = evalDivision (execWriterT (evalStateT solverCore (0, 10))) lazyPrimeDivisionUnion 1000000

solverCore :: Solver ()
solverCore = do
 (numberOfFoundPrimesLeft, actNum) <- get
 when (numberOfFoundPrimesLeft < 11) (do 
  isTruncatablePr <- isTruncatablePrime actNum
  when (isTruncatablePr) (do
   tell [actNum]
   modify (\(c, actN) -> (c + 1, actN)))
  modify (\(c, actN) -> (c, actN + 1))
  solverCore
  )