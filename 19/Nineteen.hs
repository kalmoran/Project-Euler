module Nineteen where

newtype ModSevenClass = MS Integer
 deriving (Show, Eq)

instance Num ModSevenClass where
 (+) (MS a) (MS b) = MS $ if (a + b >= 7) then a + b - 7 else a + b
 (*) (MS a) (MS b) = MS $ rem (a * b) 7
 abs (MS a)        = MS $ abs a
 signum (MS a)     = MS $ signum a
 fromInteger a 
  | a < 0     = error "ModSevenClass::fromInteger: negative Integer"
  | a < 7     = MS a
  | otherwise = MS $ rem a 7
 (-) (MS a) (MS b) = MS $ if (a - b < 0) then (7 - b + a) else a - b

getFebLengthFromYear y = 
 if (mod y 4 == 0) 
 then
  if (mod y 100 == 0)
  then
   if (mod y 400 == 0)
   then 29
   else 28
  else 29   
 else 28

getMonthsFromYear y =
 [ 31
 , (getFebLengthFromYear y)
 , 31
 , 30
 , 31
 , 30
 , 31
 , 31
 , 30
 , 31
 , 30
 , 31
 ]

processMonth nDays (s, n) = 
 (,) (s + fromInteger nDays)
  (if (s == (MS 6)) {- equals with s + 1 == 0 -}
   then n + 1
   else n) 

solver startYear startMod endYear =
 let (endMod {- Dec. 31. -}, luckySundays) = foldr processMonth (startMod, 0) (getMonthsFromYear startYear) in
  if (startYear < endYear) 
  then
   luckySundays + solver (startYear + 1) endMod endYear
  else
   luckySundays
   

