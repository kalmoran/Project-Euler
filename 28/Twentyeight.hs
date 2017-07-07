module Twentyeight where

import Control.Monad.Trans.State

solver cap = evalState (mapM calculateNextSpiral [2,4 .. cap] >>= return . (+1) . sum) 1

calculateNextSpiral stepLength = do
 lastN <- get
 put (lastN + 4 * stepLength)
 return $ 4 * lastN + 10 * stepLength 
