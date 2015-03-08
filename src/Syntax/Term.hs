{- This file is part of Razor.

  Razor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Razor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Razor.  If not, see <http://www.gnu.org/licenses/>.

  Module      : Syntax.Term
  Description : This module provies the basic data structures to capture terms.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Syntax.Term ( FnSym, Variable (..), Constant (..), Element (..)
                   , Term (..), SkolemTerm, Sub, ExistsSub, ConsSub
                   , emptySub, emptyExistsSub, emptyConsSub
                   , TermBased (..), termDepth
                   , variant, freshVariable, freshConstant, freshElement
                   , isVariable, isElement, isConstant
                   , termToVariable, termToConstant, termToElement, readElement
                   , parseTerm, pTerm, pTermList -- for parsing terms
                   , xpTerm, xpTermList
                   ) where

import Syntax.ITerm