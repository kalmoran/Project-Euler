module Fortythree where

import Combinatory
import Data.List
import DigitOperations

getAllPanDigitalNumbers = traverseList generateTenDigitNums [1 .. 9]

generateTenDigitNums ini act tai = (getAllPermutations (0 : (ini ++ tai)))

solver = length $ filter (isSubStringsDivisible) getAllPanDigitalNumbers

firstSevenPrimes = [2,3,5,7,11,13,17]

isSubStringsDivisible panDigitNum = and (isSubStringDivisible firstSevenPrimes panDigitNum)

isSubStringDivisible [] _          = []
isSubStringDivisible (pr: prs) num = ((mod (digitListToNum (take 3 num)) pr) == 0) : (isSubStringDivisible prs (tail num)) 