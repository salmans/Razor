module Tools.Logger where

import Control.Monad.Writer

logM :: (Show a) => String -> a -> Writer [String] a
logM t x = writer (x, [t ++ " ==> " ++ (show x)])