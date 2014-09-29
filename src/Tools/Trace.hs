{-| 
  Razor
  Module      : Tools.Trace
  Description : Exports a set of helper functions for tracing the code.
  Maintainer  : Salman Saghafi -}

module Tools.Trace ( module DebugTrace
                   , traceList, traceIf, traceListIf
                   , traceEval, traceEvalWith
                   , traceStringList, traceStringListIf) where

import Tools.ITrace
import Debug.Trace as DebugTrace