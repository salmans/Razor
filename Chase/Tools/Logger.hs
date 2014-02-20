module Chase.Tools.Logger (Chase.Tools.Logger.log, 
                     logUnder,
                     logIf, 
                     logUnderIf) where

import Control.Monad.RWS
import Chase.Problem.Structures

-- Salman: consider moving this insider Chase module
logUnder :: (Show a) => a -> String -> ProbPool ()
logUnder x lbl = tell [lbl ++ " ==> " ++ (show x)]

log :: (Show a) => a -> ProbPool ()
log x = tell [show x]

lIf :: Bool -> ProbPool () -> ProbPool ()
lIf cond x = if cond then x else return ()

logIf :: (Show a) => Bool -> a -> ProbPool ()
logIf cond x = lIf cond (Chase.Tools.Logger.log x)

logUnderIf :: (Show a) => Bool -> String -> a -> ProbPool ()
logUnderIf cond lbl x = lIf cond (logUnder x lbl)
