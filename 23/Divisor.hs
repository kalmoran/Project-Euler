module Divisor where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader hiding (ask)
import Control.Monad.Trans.State hiding (put, get, gets)
import Data.Array.IO
import Data.List
import Data.Monoid

type Prime   = Integer
type Divisor = Integer

data DivisionData = Uncalculated | Pr { nextPrime :: Prime } | DIVS { divisors :: [Divisor] } deriving Show 

data Array a = Arr { divisorArray :: IOArray Integer DivisionData
                   , userArray    :: IOArray Integer (Maybe a)
                   }

data DivisionStateData a = DSD { lastPrime :: Prime
                                , array    :: Array a
                                }

type UnionF        a = Integer -> Integer -> DivisionState a [Divisor] 
type DivisionState a = StateT (DivisionStateData a) IO
type Division      a = ReaderT (UnionF a)  (DivisionState a)

newDivisionState :: Integer -> IO (DivisionStateData a) 
newDivisionState cap = do
 divArr <- newArray (1, cap) Uncalculated
 usrArr <- newArray (1, cap) Nothing
 writeArray divArr 1 (Pr 2)
 return (DSD 1 (Arr divArr usrArr))

commonWrite :: (Array a -> IOArray Integer b) -> Integer -> b -> Division a ()
commonWrite selector n dat = do
 arr <- gets array
 liftIO $ writeArray (selector arr) n dat

writeDivisors       = commonWrite divisorArray
writeUserData n dat = commonWrite userArray n (Just dat) 

commonRead :: (Array a -> IOArray Integer b) -> Integer -> DivisionState a b
commonRead selector n = do
 arr <- gets array
 liftIO $ readArray (selector arr) n

readDivisors = commonRead divisorArray

readUserData :: Integer -> Division a (Maybe a)
readUserData n = lift $ commonRead userArray n

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

properDivisorUnion :: UnionF a
properDivisorUnion pr div2 = do
 divisionData <- readDivisors div2
 divs <- (case (divisionData) of
  (Pr _)    -> return [1]
  (DIVS ds) -> return ds
  (_)       -> error "properDivisorUnion: uncalculated d2")
 return $ unionSortedLists [1, div2] $ unionSortedLists divs $ map (* pr) divs

primeDivisorUnion :: UnionF a
primeDivisorUnion pr d2 = do
 divisionData <- readDivisors d2
 case (divisionData) of
  (Pr _)      -> return [pr, d2] -- order matters! pr <= d2 provided by findFirstPrimeDivisor
  (DIVS divs) -> return $ mergeSortedLists [pr] divs -- order matters here as well: pr <= x | x <- divs
  (_)         -> error "primeDivisionUnion: uncalculated d2"

getNextPrime :: Prime -> Division a Integer
getNextPrime pr = do
 divisionData <- lift $ readDivisors pr
 case (divisionData) of
  (Pr nextPr) -> return nextPr
  (_)         -> error "getNextPrime: input Integer was not a calculated prime!"

findFirstPrimeDivisor :: Integer -> Division a (Maybe (Prime, Integer))
findFirstPrimeDivisor 1      = return Nothing -- 1 is not a prime in theory, we can treat it as one
findFirstPrimeDivisor 2      = return Nothing -- findFirstPrimeDivisors' doesn't work with 2
findFirstPrimeDivisor number = findFirstPrimeDivisor' (ceiling (sqrt (fromIntegral number)))  number 2 where
 findFirstPrimeDivisor' c n d = 
  if (d > c) 
  then return Nothing
  else 
   let (d2, m) = quotRem n d in
    if (m == 0) 
    then return $ Just (d, d2)
    else do
     nextPrime <- getNextPrime d
     case (nextPrime) of
      (-1)  -> return Nothing
      ( _ ) -> findFirstPrimeDivisor' c n nextPrime


calculateDivisors :: Integer -> Division a () -- [Divisor]
calculateDivisors n = do
 (DSD lastPr arr) <- get
 el <- liftIO $ readArray (divisorArray arr) n
 case (el) of
  (Uncalculated) -> do
   foundFirstPrimeDivisor <- findFirstPrimeDivisor n
   case (foundFirstPrimeDivisor) of
    (Nothing)       -> do
     writeDivisors lastPr (Pr n)
     writeDivisors n (Pr (-1))
     put (DSD n arr)
     --return []
    (Just (pd, d2)) -> do
     divisorUnionF <- ask
     divisors      <- lift $ divisorUnionF pd d2
     writeDivisors n (DIVS divisors)
     --return divisors
  (_) -> return ()
  {- (Pr _)   -> return []
     (DIVS ds) -> return ds -}
 
iterationCore processor cap = mapM (\n -> calculateDivisors n >> processor n) [1 .. cap]


-- monoid prototype iterator
iterateOnInterval :: Monoid b => (Integer -> Division a b) -> UnionF a -> Integer -> IO b
iterateOnInterval processor unionF cap = 
 newDivisionState cap >>= 
 evalStateT (runReaderT (liftM mconcat (iterationCore processor cap)) unionF)

execStateOfInterval :: (Integer -> Division a b) -> UnionF a -> Integer -> IO (Array a)
execStateOfInterval processor unionF cap = 
 newDivisionState cap >>= 
 execStateT (runReaderT (iterationCore processor cap) unionF) >>=
 return . array


printContents :: Integer -> Division () ()
printContents n = lift (readDivisors n) >>= liftIO . putStrLn . (\x -> (show n) ++ ": " ++ x ) . show
 
debugger = iterateOnInterval printContents  

