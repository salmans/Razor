{-| 
  Razor
  Module      : Syntax.Geometric
  Description : The module defines the syntax of geometric formulas, sequents
  and theories.
  Maintainer  : Salman Saghafi -}
module Syntax.Geometric ( RelSym
                        , Atom (..), Formula (..), Sequent (..), Theory
                        , pTheory, xparseSequent, xparseFormula, pSkolemFunction
                        , parseGeometricTheory, parseSequent, parseFormula) where

import Syntax.IGeometric