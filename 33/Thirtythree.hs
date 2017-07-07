module Thirtythree where

import Data.List

data NumDenPair = NDP Int Int deriving Show

instance Eq NumDenPair where
 (==) (NDP num1 den1) (NDP num2 den2) = num1 * den2 == num2 * den1

solver = concat $ concat $ map (\x -> map (tryAllReductions x) [1 .. (x - 1)]) [2 .. 9]

tryAllReductions den num = concat $ map (\x -> tryOneReduction den num x) [1 .. 9]

tryOneReduction den num extraNum = (left den num extraNum) ++ (right den num extraNum)

twoDigNum a b = a * 10 + b

checkForEq ndp1 ndp2
 | ndp1 == ndp2 = [ndp1]
 | otherwise    = []
 
left  den num extraNum = checkForEq (NDP num den) (NDP (twoDigNum extraNum num) (twoDigNum den extraNum)) 
right den num extraNum = checkForEq (NDP num den) (NDP (twoDigNum num extraNum) (twoDigNum extraNum den))

--the solution is 4 * 5 * 5 * 2 = 200 (1/4, 1/5, 2/5, 4/8) 