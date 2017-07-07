module TwentyNine where

import BinarySearchTree
import Divisor

type PowerBase = Integer
type Exponent  = Integer

data BaseExpPair = BEP PowerBase Exponent

instance Eq BaseExpPair where
 (==) (BEP b1 e1) (BEP b2 e2) = (==) b1 b2 && (==) e1 e2

instance Ord BaseExpPair where
 compare (BEP b1 e1) (BEP b2 e2) = 
  case (compare b1 b2) of
   (EQ)     -> compare e1 e2
   (other)  -> other 


