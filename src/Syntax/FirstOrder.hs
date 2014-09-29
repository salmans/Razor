{-| 
  Razor
  Module      : Syntax.FirstOrder
  Description : The module defines the syntax of first-order formulas.
  Maintainer  : Salman Saghafi -}
module Syntax.FirstOrder ( RelSym
                         , Atom (..), Formula (..)
                         , parseFormula) where

import Syntax.IFirstOrder