module TwentyTwo where

import Control.Monad
import Data.Array
import Data.List

scores :: Array Char Integer
scores = array ('A', 'Z') (zip ['A'..'Z'] [1..])

scoreName :: String -> Integer
scoreName = sum . (map (scores !))

solver = do
 raw   <- readFile "names.txt"
 names <- return $ map (filter (\x -> not ((x == ',') || (x == '\"')))) $ groupBy (\_ y -> not (y ==',')) raw
 return $ sum $ map (\(x, y) -> x * y) $ zip [1..] $ map (scoreName) $ sort names
 


