{- Razor
   Module      : Tools.ITrace
   Description : Implements helper functions for tracing.
   Maintainer  : Salman Saghafi -}

module Tools.ITrace where

import Debug.Trace

traceList :: (Show a) => [a] -> b -> b
traceList itms x = foldr traceShow x itms

traceListIf :: (Show a) => Bool -> [a] -> b -> b
traceListIf cond itms x = 
    if cond 
    then traceList itms x 
    else x

traceEval :: Show a => a -> a
traceEval x =  traceShow x x

traceEvalWith :: Show b => (a -> b) -> a -> a
traceEvalWith f x = traceShow (f x) x

traceIf :: Show a => Bool -> a -> b -> b
traceIf cond x y = if cond then (traceShow x) y else y

traceStringList :: [String] -> b -> b
traceStringList itms x = foldr trace x itms

traceStringListIf :: Bool -> [String] -> b -> b
traceStringListIf cond itms x = if cond 
                                then traceStringList itms x 
                                else x
