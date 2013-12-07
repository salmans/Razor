module Tools.Logger where

import Control.Monad.RWS
import Chase.Problem.Structures

-- Salman: consider moving this insider Chase module
logM :: (Show a) => String -> a -> ProbPool a
logM t x = writer (x, [t ++ " ==> " ++ (show x)])