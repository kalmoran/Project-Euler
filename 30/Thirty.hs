module Thirty where

import Data.Ix
import Data.List
import Divisor
import BinarySearchTree

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader hiding (ask, asks)
import Control.Monad.Trans.State  hiding (put, get, gets)
import Control.Monad.Trans.Writer hiding (tell)
import Control.Monad.Identity


firstNum  = [0,1] -- ~ 10
firstDiff = 9 -- 10 - 1



data NumDifPair = NDP { numList    :: [Integer] 
                      , actValue   :: Integer
                      , difference :: Integer} deriving Show


firstNDP = NDP firstNum 10 firstDiff


newtype Sum = S Integer deriving Show


instance Monoid Sum where
 mempty              = S 0
 mappend (S a) (S b) = S (a + b)


data Environment = Env { nthPowDiffs :: [Integer]
                       , nineOnNthPow :: Integer
                       , estimatedSup :: Integer
                       }
 
 
type DigitNthPowers = ReaderT Environment (StateT NumDifPair (WriterT Sum Identity))


calculateNthPowDiffs n = [ x ^ n - (x - 1) ^ n | x <- range (1,9)]


lastNthLongNum n = foldr (\a b -> b * 10 + a) 0 (replicate (fromIntegral n) 9) 


estimateEnd n = (fst . head . dropWhile (\(a,b) -> a > b) . map (\x -> (x*9^n, lastNthLongNum x))) [2,3..] 


solver n = execWriterT (evalStateT (runReaderT solverCore (Env (calculateNthPowDiffs n) (9 ^ n) (estimateEnd n))) firstNDP) 


solverCore :: DigitNthPowers ()
solverCore = do
 ndp      <- get
 upperLim <- asks estimatedSup
 when ((actValue ndp) < upperLim) (do
  --tell [ndp]
  when ((difference ndp) == 0) (tell (S (actValue ndp)))
  increaseNum ndp >>= put
  solverCore)


increaseNum :: NumDifPair -> DigitNthPowers NumDifPair 
increaseNum (NDP num actV diff) = do
 (Env nthDiffs nineNthPow _) <- ask
 case num of 
  ([])   -> return $ NDP [1] (actV + 1) diff -- diff - 1 + 1
  (9:xs) -> do
    (NDP newNum newActV newDiff) <- increaseNum (NDP xs actV diff)
    return $ NDP (0:newNum) newActV (newDiff + nineNthPow)
  (a:xs) -> return $ NDP ((a + 1):xs) (actV + 1) (diff - (nthDiffs !! (fromIntegral a)) + 1)