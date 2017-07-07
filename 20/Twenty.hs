module Twenty where

import Data.List
import Data.Tuple

factorial n = product [1 .. n]

getDigits :: Integer -> [Integer]
getDigits = unfoldr (\x -> 
 if (x > 0) 
 then Just $ swap $ quotRem x 10
 else Nothing) 

solver = sum . getDigits . factorial
