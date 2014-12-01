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

  Module      : Chase.PossibleFacts.RelAlg.DB
  Description : Provides an interface for interacting with the database
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Chase.PossibleFacts.RelAlg.DB ( Set(..)
                                    , Select(..), Project(..)
                                    , select, project, join
                                    , fromList, toList
                                    , empty, singleton, oneMember, insert
                                    , IDB.null, IDB.elem, nub, size
                                    , IDB.map, IDB.filter
                                    , union, unions, difference ) where

{- Select DB implementation: -}
-- import Chase.PossibleFacts.RelAlg.IListDB as IDB -- based on Haskell lists
import Chase.PossibleFacts.RelAlg.ISetDB as IDB -- based on Data.Set
-- import Chase.PossibleFacts.RelAlg.IHashSetDB as IDB -- based on Data.HashSet
