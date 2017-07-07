module Thirtytwo where


import Data.List

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader hiding (ask, asks)
import Control.Monad.Trans.State  hiding (put, get, gets)
import Control.Monad.Trans.Writer hiding (tell)
import Control.Monad.Identity


data Sum = Sum Integer

instance Monoid Sum where
 mempty                  = Sum 0
 mappend (Sum a) (Sum b) = Sum $ a + b 

data SolverState = SS {digitsThatNeedProcession :: [Bool]}

type Solver = Writer Sum --StateT SolverState (Writer Sum Identity)

execSolver comp = execWriter comp --execStateT (execWriterT comp) inState


traverseList :: ([a] -> a -> [a] -> [b]) -> [a] -> [b]
traverseList f []     = []
traverseList f (x:xs) = traverseList' f [] x xs


traverseList' :: ([a] -> a -> [a] -> [b]) -> [a] -> a -> [a] -> [b] 
traverseList' f ini x [] = f ini x []
traverseList' f ini x xs = (f ini x xs) ++ (traverseList' f (ini ++ [x]) (head xs) (tail xs))   


getAllPermutations :: [a] -> [[a]]
getAllPermutations l = getAllVariations (length l) l 


getAllVariations :: Int -> [a] -> [[a]]
getAllVariations 0 _       = [[]]
getAllVariations length [] = [[]]
getAllVariations length l  = traverseList (\ini act tai -> map (act :) (getAllVariations (length - 1) (ini ++ tai))) l


fact 0 = 1
fact a = product [1 .. a]


listToNum []     = 0
listToNum (x:xs) = x + 10 * (listToNum xs)  


type Result = Int


solver = product $ map toInteger $ nub $ firstNumIsOneDigit ++ firstNumIsTwoDigits
 


firstNumIsOneDigit :: [Result]
firstNumIsOneDigit = traverseList (oneDigitSolverCore) [1..9]

prepareTwoDigits = map (\l -> (fst (head l), map snd l)) (groupBy (\x y -> fst x == fst y) (map (splitAt 2) (getAllVariations 5 [1..9])))

firstNumIsTwoDigits :: [Result]
firstNumIsTwoDigits = runIdentity $ execWriterT (
 mapM_ (\(h, l) -> evalStateT (runReaderT recursiveTwoDigitsSolverCore (h, digitListToNum h)) l) prepareTwoDigits
 ) 
 

recursiveTwoDigitsSolverCore = do
 l <- get
 when (length l > 0) (do
  (act, actV) <- ask
  (x:xs)      <- return l
  xValue      <- return $ digitListToNum x
  product     <- return $ xValue * actV
  when (product < 10000) (do
   put xs
   when ((sort (act ++ x ++ (numToDigitList product))) == [1 .. 9]) (tell [product])
   recursiveTwoDigitsSolverCore))
 


 
oneDigitSolverCore :: [Int] -> Int -> [Int] -> [Result]
oneDigitSolverCore _ 1 _       = []
oneDigitSolverCore ini act tai = runIdentity $ execWriterT (evalStateT (runReaderT recursiveOneDigitSolverCore act) (getAllVariations 4 (ini ++ tai)))

digitListToNum = foldl (\b a -> b * 10 + a) 0

numToDigitList :: Int -> [Int]
numToDigitList x = unfoldr (checkForZero . ((flip quotRem) 10)) x

checkForZero (div, rem)
 | div == 0 && rem == 0 = Nothing
 | otherwise            = Just (rem, div)  


recursiveOneDigitSolverCore :: ReaderT Int (StateT [[Int]] (WriterT [Result] Identity)) ()
recursiveOneDigitSolverCore = do
 act <- ask
 l   <- get
 when ((length l) > 0) (do  
  (x:xs)  <- return l
  put xs
  xValue  <- return $ digitListToNum x
  product <- return $ xValue * act
  when (product < 10000) (do
   prodList <- return $ numToDigitList product
   when ((sort ([act] ++ x ++ prodList)) == [1..9]) (tell [product])
   recursiveOneDigitSolverCore))