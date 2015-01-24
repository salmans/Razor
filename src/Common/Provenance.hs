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

  Module      : Common.Provenance
  Description : Defines provenance information and offers functions to work with
  provenance information.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}
module Common.Provenance ( Blame (..), ObservationProvs, ProvInfo (..)
                         , ElementProvs
                         , addElementProv, getElementProv, testElementProv
                         , testElementProvWithDepth
                         , findElementWithProv, findObservationWithProv
                         , addBlameSequent, findBlameSequent
                         , modifyElementProvs, modifyBlameSequentMap
                         , emptyProvInfo, emptyProvInfoWithConstants ) where

import Common.IProvenance