module Seventeen where

import Data.List

divideAndCount num divider = 
 let (d, m) = quotRem num divider in
  countVowels d + countVowels m

countVowels x -- x < 1000000 
 | x < 20 = case (x) of
  (0)  -> 0
  (1)  -> 3
  (2)  -> 3
  (3)  -> 5
  (4)  -> 4
  (5)  -> 4
  (6)  -> 3
  (7)  -> 5
  (8)  -> 5
  (9)  -> 4
  (10) -> 3
  (11) -> 6
  (12) -> 6
  (13) -> 8
  (14) -> 8
  (15) -> 7
  (16) -> 7
  (17) -> 9
  (18) -> 9
  (19) -> 8 
 | {- 20 <= -} x < 100 = 
  let (d, m) = quotRem x 10 in
   countVowels m + (case (d) of
    (2) -> 6
    (3) -> 5
    (4) -> 6
    (5) -> 5
    (6) -> 5
    (7) -> 7
    (8) -> 7
    (9) -> 6)
 | {- 100 <= -}  x < 1000    = (divideAndCount x 100) + 10 -- ... houndred and ...
 | {- 1000 <= -} x < 1000000 = (divideAndCount x 1000) + 8 -- ... thousand ...
 | otherwise                 = error ("unimplemented for this number: " ++ show (x))

generateListTillCap cap = unfoldr (\x -> 
 if (x <= cap)
 then Just (x, x + 1)
 else Nothing) 1

solver cap = sum $ map countVowels $ (generateListTillCap cap)
