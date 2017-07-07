module Fortyfive where

import Control.Monad.Trans.State
import Data.Either

getNthTriangleNum n = div (n * (n + 1)) 2
getNthPentNum     n = div (n * (3 * n - 1)) 2
getNthHexNum      n = n * (2 * n - 1)

type GreaterNum = Integer
type EqualNum   = Integer

type NumNPair   = (Integer, Integer)

data NumGenerationResult = Either NumNPair NumNPair

makeResult target (num, n) = 
 if (num == target)
 then Right (num, n)
 else Left  (num, n)

unPackEither (Left a)  = a
unPackEither (Right a) = a

generateNumUpToTarget generator target n = (makeResult target . head . (dropWhile ((< target) . fst)) . (map (\x -> (generator x, x))))  [n ..]
generatePentUpToTarget = generateNumUpToTarget getNthPentNum
generateHexUpToTarget  = generateNumUpToTarget getNthHexNum
generateTriUpToTarget  = generateNumUpToTarget getNthTriangleNum

solver = runState solverCore (285, 165, 143)

solverCore = do
 (triN, penN, hexN) <- get
 newTarget          <- return $ getNthHexNum $ hexN + 1
 resTri             <- return $ generateTriUpToTarget newTarget triN 
 resPent            <- return $ generatePentUpToTarget newTarget penN
 if (isRight resTri && isRight resPent) 
 then return newTarget
 else do
  put ((snd . unPackEither) resTri, (snd . unPackEither) resPent , hexN + 1)
  solverCore
 
 
