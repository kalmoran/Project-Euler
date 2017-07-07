module Fortytwo where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Array
import Data.List
import Data.Maybe

import BinarySearchTree

readInput inputFile = do
 raw <- readFile inputFile 
 return $ map (filter (\x -> not ((x == ',') || (x == '\"')))) $ groupBy (\_ y -> not (y ==',')) raw
 
data MaxSGTree a = MSGT { sgTree :: ScapeGoatBinaryTree a 
                        , maxVal :: a
                        }
                   
emptyMaxSGTree :: a -> MaxSGTree a
emptyMaxSGTree a = MSGT emptySGTree a

instance BinarySearchTree MaxSGTree where
 insert (MSGT sgTree oldMax) newA      = MSGT  (BinarySearchTree.insert sgTree newA) (max oldMax newA)
 delete (MSGT sgTree oldMax) aToDelete = error "unimplemeted"
 findElem a (MSGT sgTree max)          = findElem a sgTree 

 
type TriangleNum     = Int
type Count           = Int 
data TriangleNumPair = TN { count       :: Count
                          , triangleNum :: TriangleNum
                          }                       
                       deriving (Show)

instance Eq TriangleNumPair where
 (==) (TN c1 t1) (TN c2 t2) = t1 == t2
 
instance Ord TriangleNumPair where
 compare (TN c1 t1) (TN c2 t2) = compare t1 t2
 
 
getNthTriangleNum n = div (n * (n + 1)) 2

generateTriangleNums startN valueCap = takeWhile ((TN 0 valueCap) >=) $ map (\n -> TN n (getNthTriangleNum n)) [startN ..]


scores :: Array Char Int
scores = array ('A', 'Z') (zip ['A'..'Z'] [1..])

scoreWord :: String -> Int
scoreWord = sum . (map (scores !))

solver :: IO Int
solver = evalStateT solverCore (emptyMaxSGTree (TN 1 1)) >>= return . length

--solverCore :: StateT (MaxSGTree TriangleNumPair) IO Int
solverCore = liftIO (readInput "words.txt") >>= mapM inspectWord >>= return . (mapMaybe id) -- >>= return . sum

--inspectWord :: String -> StateT (MaxSGTree TriangleNumPair) IO Int
inspectWord word = do
 wordScore <- return $ scoreWord word
 msgTree   <- get
 when (triangleNum (maxVal msgTree) < wordScore) (put (foldl (BinarySearchTree.insert) msgTree (generateTriangleNums (count (maxVal msgTree)) wordScore)))
 msgTree'  <- get
 return $ (\x -> (word, x)) <$> findElem (TN 0 wordScore) msgTree'
  
 {-
 if (isJust (findElem (TN 0 wordScore) msgTree')) 
 then return 1
 else return 0
 -}
 
 