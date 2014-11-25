{-| 
  Razor
  Module      : Syntax.Term
  Description : This module provies the basic data structures to capture terms.
  Maintainer  : Salman Saghafi -}

module Syntax.Term ( FnSym, Variable (..), Constant (..), Element (..)
                   , Term (..), SkolemTerm, Sub, ExistsSub, ConsSub
                   , emptySub, emptyExistsSub, emptyConsSub
                   , TermBased (..), termDepth
                   , variant, freshVariable, freshConstant, freshElement
                   , isVariable, isConstant, termToVariable, termToConstant
                   , termToElement, readElement
                   , parseTerm, pTerm, pTermList -- for parsing terms
                   , xpTerm, xpTermList
                   ) where

import Syntax.ITerm