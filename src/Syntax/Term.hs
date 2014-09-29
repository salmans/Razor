{-| 
  Razor
  Module      : Syntax.Term
  Description : This module provies the basic data structures to capture terms.
  Maintainer  : Salman Saghafi -}

module Syntax.Term ( FnSym, Variable (..), Constant (..), Element (..)
                   , Term (..), Sub, ExistsSub, ConsSub
                   , emptySub, emptyExistsSub, emptyConsSub
                   , TermBased (..), termDepth
                   , variant, freshVariable, freshConstant, freshElement
                   , isVariable, isConstant, termToVariable, termToConstant
                   , termToElement
                   , parseTerm, pTerm, pTermList -- for parsing terms
                   ) where

import Syntax.ITerm