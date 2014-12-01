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

  Module      : Chase.PossibleFacts.PossibleFacts
  Description : Provides a container for storing facts that the Chase 
  computes. This implementation for PossibleFacts is based on relational algebra.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Chase.PossibleFacts.RelAlg.PossibleFacts
    ( ChaseSequentType, ChasePossibleFactsType ) where

import Chase.PossibleFacts.RelAlg.IPossibleFacts

type ChaseSequentType       = RelSequent
type ChasePossibleFactsType = RelPossibleFacts