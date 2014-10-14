{-| 
  Razor
  Module      : Syntax.GeometricUtils
  Description : Offers a set of helper functions to work with formulas and terms.
  Maintainer  : Salman Saghafi -}

module Syntax.GeometricUtils 
    ( module Term, module Geometric -- export these two
    , RelationBased (..)
    , simplify, preprocess, formulaExistentials, sequentExistentials
    , sequentExistsSubstitute, formulaExistsSubstitute
    ) where

import Syntax.IGeometricUtils
import Syntax.Term as Term
import Syntax.Geometric as Geometric