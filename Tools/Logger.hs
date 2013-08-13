module Logger where

import Control.Monad.Writer

log :: (Show a) => String -> a -> Writer [String] a
log t x = writer (x, [t ++ " ==> " ++ (show x)])