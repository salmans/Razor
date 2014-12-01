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

  Module      : Tools.ExtendedSet
  Description : Provides extra functions to work with sets
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Tools.ExtendedSet
    ( module Set
    , mapM
    , mapM_
    , foldM
    , filterM
    , catMaybes
    , mapMaybe
    , flatten
    , concatMap
    , concatMapM
    , any
    , all
    , or
    , and
    , ss
    , toSS
    , fromSS
    , ssMapM
    , distrib
    , cartesianProduct
    , groupBy
    , partitionM
    , unzip
    ) where

import Data.Set as Set
import Prelude hiding ( mapM, mapM_, unzip, all, any, map, filter, null
                      , concatMap, and, or)
import Tools.IExtendedSet