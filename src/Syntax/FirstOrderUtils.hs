{-| 
  Razor
  Module      : Syntax.FirstOrderUtils
  Description : Offers a set of helper functions to work with first-order 
  formulas and terms.
  Maintainer  : Salman Saghafi -}

module Syntax.FirstOrderUtils
    ( module Term, module FirstOrder -- export these two
    , simplify
    ) where

import Syntax.IFirstOrderUtils
import Syntax.Term as Term
import Syntax.FirstOrder as FirstOrder