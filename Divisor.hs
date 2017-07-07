{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds   #-}

module Divisor (
                 DivisionData(Pr,DIVS)
               , Division
               , MonadDivisor (..)
               , runDivision
               , evalDivision
               , runPrimeDivision
               , evalPrimeDivision
               , lazyPrimeDivisionUnion
               ) where

import Control.Monad.Identity
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader hiding (ask)
import Control.Monad.Trans.State hiding (put, get, gets, modify)
import Data.Array.IO
import Data.List
import Data.Monoid

type Prime   = Integer
type Divisor = Integer

data DivisionData = Uncalculated | Pr | DIVS { divisors :: [Divisor] } deriving Show 

type DivisorArray = IOArray Integer DivisionData

data DivisionStateData = DSD { primes            :: [Prime]
                             , lastCalculatedNum :: Integer
                             , array             :: DivisorArray
                             }

type DivisionState      = StateT DivisionStateData IO
type UnionF             = Prime -> Integer -> DivisionState [Divisor] 
type DivisionStack      = ReaderT UnionF (StateT DivisionStateData IO)
data Division         a = Div { runDiv :: DivisionStack a }

type MonadConstraints m = (MonadState DivisionStateData m, MonadIO m)

instance Functor Division where
 fmap f (Div a) = Div $ fmap f a
 
instance Applicative Division where
 pure = Div . pure
 (<*>) f b = Div $ runDiv f <*> runDiv b

instance Monad Division where
 return = Div . return
 (>>=) m b = Div $ runDiv m >>= runDiv . b

class Monad m => MonadDivisor m where
 getDivisors      :: Integer -> m DivisionData
 isPrime          :: Integer -> m Bool
 getPrimesTillCap :: Integer -> m [Integer]
 
instance MonadDivisor m => MonadDivisor (StateT s m) where
 getDivisors      = lift . getDivisors
 isPrime          = lift . isPrime 
 getPrimesTillCap = lift . getPrimesTillCap

instance MonadDivisor m => MonadDivisor (ReaderT r m) where
 getDivisors      = lift . getDivisors
 isPrime          = lift . isPrime 
 getPrimesTillCap = lift . getPrimesTillCap
 
instance (Monoid w, MonadDivisor m) => MonadDivisor (WriterT w m) where
 getDivisors      = lift . getDivisors
 isPrime          = lift . isPrime 
 getPrimesTillCap = lift . getPrimesTillCap

instance MonadDivisor Division where
 getDivisors      = Div . getDivisors'
 isPrime          = Div . isPrime'
 getPrimesTillCap = Div . getPrimesTillCap' 

runDivision :: Division a -> UnionF -> Integer -> IO (a, DivisionStateData)
runDivision (Div div) unionF initialCap = newDivisonState initialCap >>= runStateT (runReaderT div unionF) 

evalDivision :: Division a -> UnionF -> Integer -> IO a
evalDivision (Div div) unionF initialCap = newDivisonState initialCap >>= evalStateT (runReaderT div unionF)
 
runPrimeDivision  div cap = runDivision div primeDivisorUnion cap
evalPrimeDivision div cap = evalDivision div primeDivisorUnion cap  

 
newDivisonState :: MonadIO m => Integer -> m DivisionStateData 
newDivisonState cap = do
 divArr <- liftIO $ newArray (1, cap) Uncalculated
 liftIO $ writeArray divArr 1 Pr
 return (DSD [] 1 divArr)
 
readDivisors :: MonadConstraints m => Integer -> m DivisionData
readDivisors n = do
 arr <- gets array
 liftIO $ readArray arr n

writeDivisors :: MonadConstraints m => Integer -> DivisionData -> m ()
writeDivisors n dat = do 
 arr <- gets array
 liftIO $ writeArray arr n dat 


isPrime' :: Integer -> DivisionStack Bool
isPrime' 1 = return False
isPrime' n = do
 divData <- getDivisors' n
 case divData of
  (Pr)            -> return True
  (DIVS _)        -> return False


getPrimesTillCap' :: Integer -> DivisionStack [Integer]
getPrimesTillCap' cap = getDivisors' cap >> gets primes >>= return . reverse . (dropWhile (> cap)) . reverse

getDivisors' :: Integer -> DivisionStack DivisionData
getDivisors' n = do
 (DSD primes lastNum arr) <- get
 oldSup <- (liftIO . getBounds) arr >>= return . snd
 when (n > oldSup) (do
  newSup    <- return $ max (oldSup * 2) n
  newArray  <- liftIO $ increaseArraySizes oldSup newSup arr
  put (DSD primes lastNum newArray))
 when (lastNum < n) (mapM_ calculateDivisors [(lastNum + 1) .. n] >> modify (\(DSD primes _ arr') -> DSD primes n arr'))
 readDivisors n
  
 
properDivisorUnion :: UnionF
properDivisorUnion pr div2 = do
 divisionData <- readDivisors div2
 divs <- (case (divisionData) of
  (Pr)      -> return [1]
  (DIVS ds) -> return ds
  (_)       -> error "properDivisorUnion: uncalculated d2")
 return $ unionSortedLists [1, div2] $ unionSortedLists divs $ map (* pr) divs


primeDivisorUnion :: UnionF
primeDivisorUnion pr d2 = do
 divisionData <- readDivisors d2
 case (divisionData) of
  (Pr)        -> return [pr, d2] -- order matters! pr <= d2 provided by findFirstPrimeDivisor
  (DIVS divs) -> return $ mergeSortedLists [pr] divs -- order matters here as well: pr <= x | x <- divs
  (_)         -> error ("primeDivisionUnion: uncalculated d2: " ++ show pr ++ " " ++ show d2 )  
 
lazyPrimeDivisionUnion :: UnionF
lazyPrimeDivisionUnion pr d2 = return [pr, d2]
 

--Utilities
sortedMergeSkeleton :: Ord a => ([a] -> [a]) -> [a] -> [a] -> [a]
sortedMergeSkeleton eqHdl xs [] = xs
sortedMergeSkeleton eqHdl [] ys = ys
sortedMergeSkeleton eqHdl xl yl = 
 let ([xh], xs) = splitAt 1 xl in
  let ([yh], ys) = splitAt 1 yl in
   case (compare xh yh) of
    (LT) -> xh : sortedMergeSkeleton eqHdl xs yl
    (EQ) -> xh : sortedMergeSkeleton eqHdl xs (eqHdl yl)
    (GT) -> yh : sortedMergeSkeleton eqHdl xl ys  

unionSortedLists = sortedMergeSkeleton tail
mergeSortedLists = sortedMergeSkeleton id   


findFirstPrimeDivisor :: MonadConstraints m => Integer -> m (Maybe (Prime, Integer))
findFirstPrimeDivisor 1      = return Nothing -- 1 is not a prime in theory, we can treat it as one
findFirstPrimeDivisor 2      = return Nothing -- findFirstPrimeDivisors' doesn't work with 2
findFirstPrimeDivisor number = gets primes >>= findFirstPrimeDivisor' (ceiling (sqrt (fromIntegral number))) number

findFirstPrimeDivisor' c n []        = return Nothing
findFirstPrimeDivisor' c n (pr:prs) = 
 if (pr > c) 
 then return Nothing
 else 
  let (d2, m) = quotRem n pr in
   if (m == 0) 
   then return $ Just (pr, d2)
   else findFirstPrimeDivisor' c n prs
 

increaseArraySizes :: Integer -> Integer -> DivisorArray -> IO DivisorArray
increaseArraySizes oldCap newCap divArr = do
 newDivArr  <- newArray (1, newCap) Uncalculated
 sequence_ [ readArray divArr i >>= writeArray newDivArr i | i <- range (1, oldCap)]
 return newDivArr
  
  
calculateDivisors :: Integer -> DivisionStack ()
calculateDivisors n = do
 foundFirstPrimeDivisor <- findFirstPrimeDivisor n
 case (foundFirstPrimeDivisor) of
  (Nothing)       -> do
   writeDivisors n (Pr)
   modify (\(DSD primes lastNum arr) -> DSD (primes ++ [n]) lastNum arr)
  (Just (pd, d2)) -> do
   divisorUnionF <- ask
   divisors      <- lift $ divisorUnionF pd d2
   writeDivisors n (DIVS divisors)
   
