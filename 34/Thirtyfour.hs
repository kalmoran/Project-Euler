module Thirtyfour where

import Data.Array
import Data.List

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader hiding (ask, asks)
import Control.Monad.Trans.State  hiding (put, get, gets)
import Control.Monad.Trans.Writer hiding (tell)
import Control.Monad.Identity

fact 0 = 1
fact n = product [1 .. n]

numToDigitList :: Int -> [Int]
numToDigitList x = unfoldr (checkForZero . ((flip quotRem) 10)) x

checkForZero (div, rem)
 | div == 0 && rem == 0 = Nothing
 | otherwise            = Just (rem, div)  
 
 
solver :: [Int]
solver = runIdentity $ execWriterT $ evalStateT recursiveSolver 10

recursiveSolver :: StateT Int (WriterT [Int] Identity) ()
recursiveSolver = do
 actNum <- get
 when (actNum <= (fact 9) * 6) (do
  put (actNum + 1)
  when (actNum == (sum (map fact (numToDigitList actNum)))) (tell [actNum])
  recursiveSolver
  )