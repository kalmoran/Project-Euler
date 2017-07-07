module BinarySearchTree where

import Control.Applicative
import Control.Monad.Trans.State
import Data.Foldable
import Data.Functor
import Data.List hiding (foldr)
import Data.Traversable
import Prelude hiding (foldr)


data BinaryTree a = NIL | BT { left      :: (BinaryTree a) 
                             , nodeValue :: a 
                             , right     :: (BinaryTree a) 
                             }


type TreeSize = Integer


data ScapeGoatBinaryTree a = SG { balanceFactor :: Double
                                , nodeCount     :: Int
                                , maxNodeCount  :: Int
                                , binaryTree    :: BinaryTree a
                                } deriving Show


emptySGTree = SG (0.51) 0 0 NIL


exampleSGTree = foldr (flip insertIntoSGTree) emptySGTree [1,3,2,4,5,6,7]


data SGInsertResult a = 
   Balanced (BinaryTree a)
 | ISGT (BinaryTree a) TreeSize
 | NoChange a

data SGDeleteResult a = WasDelete (BinaryTree a) | NoDelete


class BinarySearchTree t where
 insert     :: Ord a => t a -> a -> t a
 delete     :: Ord a => t a -> a -> t a
 findElemBy :: Ord a => (a -> Ordering) -> t a -> Bool


instance Functor BinaryTree where
 fmap f NIL                  = NIL
 fmap f (BT left elem right) = BT (fmap f left) (f elem) (fmap f right) 


instance Foldable BinaryTree where
 foldr f bv NIL                = bv
 foldr f bv (BT left av right) = foldr f (f av (foldr f bv right)) left


instance Traversable BinaryTree where
 traverse f NIL                = pure NIL
 traverse f (BT left av right) = BT <$> traverse f left <*> f av <*> traverse f right


instance Show a => Show (BinaryTree a) where
 show NIL        = "NIL"
 show (BT l a r) = "BT (" ++ (show l) ++ ") " ++ (show a) ++ " (" ++ (show r) ++ ")" 


instance BinarySearchTree ScapeGoatBinaryTree where
 insert          = insertIntoSGTree
 delete          = deleteFromSGTree
 findElemBy comp = (findElemInBinaryTreeBy comp) . binaryTree


findElemInBinaryTreeBy :: Ord a => (a -> Ordering) -> BinaryTree a -> Bool
findElemInBinaryTreeBy comp NIL                  = False
findElemInBinaryTreeBy comp (BT left elem right) = 
 case (comp elem) of
 (LT) -> findElemInBinaryTreeBy comp right
 (EQ) -> True
 (GT) -> findElemInBinaryTreeBy comp left


buildFromSortedList :: Ord a => [a] -> BinaryTree a
buildFromSortedList []     = NIL
buildFromSortedList [x]    = BT NIL x NIL
buildFromSortedList ls     = 
 let (l, medAndRight) = splitAt (div (length ls) 2) ls in 
  let (medl, r) = splitAt 1 medAndRight in
   BT (buildFromSortedList l) (head medl) (buildFromSortedList r)


binaryTreeToList :: BinaryTree a -> [a]
binaryTreeToList NIL               = []
binaryTreeToList (BT left x right) = (binaryTreeToList left) ++ [x] ++ (binaryTreeToList right)
 
 
getTreeSize :: BinaryTree a -> Integer
getTreeSize NIL               = 0
getTreeSize (BT left x right) = (getTreeSize left) + (getTreeSize right) + 1


insertIntoSGTree :: Ord a => ScapeGoatBinaryTree a -> a -> ScapeGoatBinaryTree a
insertIntoSGTree (SG bf nc mnc bt) newA = case (insertIntoSGBST bf newA bt) of
 (Balanced newTree)  -> SG bf (nc + 1) (max (nc + 1) mnc) newTree
 (ISGT newT newSize) -> SG bf (nc + 1) (max (nc + 1) mnc) newT
 (NoChange _)        -> SG bf nc mnc bt


data TreeOperationSide = L | R | Unset


treeSideCaseHandler fl fr = do
 side <- get
 case side of
  (L) -> return fl
  (R) -> return fr 


selectOperatedSubTree bt = treeSideCaseHandler (left bt) (right bt) 


putBackSubTreeIntoTree (BT left actV right) subT = (treeSideCaseHandler (BT subT actV right) (BT left actV subT)) 


insertIntoSubTree bf newA bt = do
 subTree          <- selectOperatedSubTree bt
 newSubTreeResult <- return $ insertIntoSGBST bf newA subTree
 case (newSubTreeResult) of
  (Balanced balancedNewSubTree)    -> 
   putBackSubTreeIntoTree bt balancedNewSubTree >>= (return . Balanced)
  (NoChange oldA)                  -> return $ NoChange oldA
  (ISGT newSubTree newSubTreeSize) -> do
   otherSubTree <- treeSideCaseHandler (right bt) (left bt)
   newTreeSize  <- return $ (getTreeSize otherSubTree) + newSubTreeSize + 1
   if ((fromIntegral newTreeSize) * bf < (fromIntegral newSubTreeSize))
   then do
    newTreeList <- 
     treeSideCaseHandler (binaryTreeToList newSubTree ++ [nodeValue bt] ++ binaryTreeToList (right bt))
                         (binaryTreeToList (left bt)  ++ [nodeValue bt] ++ binaryTreeToList newSubTree)
    return $ Balanced $ buildFromSortedList newTreeList
   else (putBackSubTreeIntoTree bt newSubTree) >>= (return . (\x -> ISGT x newTreeSize)) 


getTreeOperationSide compRes = 
 case compRes of
  (LT) -> R
  (GT) -> L


insertIntoSGBST :: Ord a => Double -> a -> BinaryTree a -> SGInsertResult a
insertIntoSGBST bf newA NIL = ISGT (BT NIL newA NIL) 1
insertIntoSGBST bf newA bt  = 
 let comparisonResult = compare (nodeValue bt) newA in
  if (comparisonResult == EQ) 
  then NoChange (nodeValue bt)
  else evalState (insertIntoSubTree bf newA bt) (getTreeOperationSide comparisonResult)


pullLeftMostDeepestChild NIL NIL                      = NIL
pullLeftMostDeepestChild NIL (BT leftT actV rightT)   = BT NIL actV (pullLeftMostDeepestChild leftT rightT)
pullLeftMostDeepestChild (BT leftSubT actV rightSubT) rightT = BT (pullLeftMostDeepestChild leftSubT rightSubT) actV rightT


deleteFromSGBST NIL _         = NoDelete
deleteFromSGBST bt  aToDelete = 
 case (compare (nodeValue bt) aToDelete) of
  (EQ)      -> WasDelete (pullLeftMostDeepestChild (left bt) (right bt))
  (compRes) -> evalState (deleteFromSubTree aToDelete bt) (getTreeOperationSide compRes)


deleteFromSubTree aToDelete bt = do
 subTreeToDeleteFrom <- selectOperatedSubTree bt
 case (deleteFromSGBST subTreeToDeleteFrom aToDelete) of
  (NoDelete)             -> return NoDelete
  (WasDelete newSubTree) -> putBackSubTreeIntoTree bt newSubTree >>= return . WasDelete


deleteFromSGTree :: Ord a => ScapeGoatBinaryTree a -> a -> ScapeGoatBinaryTree a
deleteFromSGTree (SG bf nc mnc bt) aToDelete = 
 case (deleteFromSGBST bt aToDelete) of
  (NoDelete)          -> SG bf nc mnc bt
  (WasDelete newTree) -> 
   if (fromIntegral (nc - 1) <= (fromIntegral mnc) * bf) 
   then SG bf (nc - 1) (nc - 1) $ buildFromSortedList $ binaryTreeToList newTree
   else SG bf (nc - 1) mnc newTree 

