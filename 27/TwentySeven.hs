module TwentySeven where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Array.IO
import Divisor

data QuadraticResolution = QR { consPrimeNum :: Integer
                              , cofA         :: Integer
                              , cofB         :: Integer} deriving Show


instance Eq QuadraticResolution where
 (==) a b = (==) (consPrimeNum a) (consPrimeNum b)
 

instance Ord QuadraticResolution where
 compare a b = compare (consPrimeNum a) (consPrimeNum b)
 

isPrime num = do
 dsd     <- get
 newDSD  <- liftIO $ lazyPull primeDivisorUnion dsd num
 --liftIO $ getBounds ((divisorArray . array) dsd) >>= putStrLn . show . snd
 numData <- liftIO $ readArray (divisorArray (array newDSD)) num
 put newDSD
 case numData of
  (Pr _) -> return True
  (_)    -> return False


findPrimeChainLength a b = do
 --liftIO $ putStrLn $ "(" ++ show a ++ ", " ++ show b ++ "):"
 dsd                      <- get
 (chainLength, resultDSD) <- runStateT (runReaderT (iterateOnNs 0) (a, b)) dsd
 put resultDSD
 return $ QR chainLength a b


iterateOnNs n = do
 (a, b) <- ask
 res    <- return $ n * n + n * a + b
 if (res < 2) 
 then return n
 else do
  isResPrime <- lift $ isPrime res
  if (isResPrime)
  then do
   --liftIO $ putStrLn $ show n ++ ": " ++ show res  
   iterateOnNs (n + 1)
  else return n


findLongestChainCofsProd absCapA absCapB = do
 chainLengths <- sequence  [findPrimeChainLength a b | a <- [(-absCapA) .. absCapA], b <- [(-absCapB) .. absCapB]]
 return ((maximum chainLengths))

--(\(QR _ a b) -> a * b) 

solver absCapA absCapB = do 
 dsd <- getDivisors primeDivisorUnion (absCapA * absCapB)
 evalStateT (findLongestChainCofsProd absCapA absCapB) dsd

debugger2 a b = newOnlyDivisorState 1 >>= evalStateT (findPrimeChainLength a b)






