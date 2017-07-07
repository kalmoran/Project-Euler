module Fifteen where

import Data.List

baseNum = 2 ^ 1000

nextListElemGenerator x =
 if (x > 0) 
 then Just (mod x 10, div x 10)
 else Nothing

solver = sum (unfoldr nextListElemGenerator baseNum) 
 


