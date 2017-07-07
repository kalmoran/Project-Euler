module Thirtysix where

import Combinatory
import DigitOperations
import Data.Int
import Data.List


type BinaryDigitList = [Int8]


checkForZero (div, rem)
 | div == 0 && rem == 0 = Nothing
 | otherwise            = Just (fromIntegral rem, div) 


intToBinaryList :: Integral a => a -> BinaryDigitList
intToBinaryList n = reverse $ unfoldr (checkForZero . ((flip quotRem 2))) n


isPalindromicBinary :: BinaryDigitList -> Bool
isPalindromicBinary l = and $ map (\(x,y) -> x == y) $ zip l (reverse l)


getNLengthPalidroms n = 
 let (div, rem) = quotRem n 2 in 
  if (rem == 0)
  then
   let halfPals = concat $ map (\x -> map (x:) (getAllVariationsWithRepeate (div - 1) [0..9])) [1 .. 9] in
    map (\x -> x ++ reverse x) halfPals
  else
   let halfPals = concat $ map (\x -> map (x:) (getAllVariationsWithRepeate div [0..9])) [1 .. 9] in
    map (\x -> x ++ (tail . reverse) x) halfPals


getAllPalindromsTillNLength :: Int -> [[Int]]
getAllPalindromsTillNLength n = concat $ map getNLengthPalidroms [2 .. n]


solver = (sum . (fst <$>)) $ filter (isPalindromicBinary . snd) $ (\x-> (x, intToBinaryList x)) <$> digitListToNum <$> getAllPalindromsTillNLength 6 


