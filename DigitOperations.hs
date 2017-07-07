module DigitOperations( 
   digitListToNum
 , numToDigitList) where

import Data.List
 
digitListToNum :: Integral a => [a] -> a 
digitListToNum = foldl (\b a -> b * 10 + a) 0

numToDigitList :: Integral a => a -> [a]
numToDigitList x = reverse $ unfoldr (checkForZero . ((flip quotRem) 10)) x

checkForZero (div, rem)
 | div == 0 && rem == 0 = Nothing
 | otherwise            = Just (rem, div) 