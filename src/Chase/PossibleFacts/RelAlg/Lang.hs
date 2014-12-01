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

  Module      : Chase.PossibleFacts.RelAlg.Lang
  Description : Provides the language of relational algebra for evaluating 
  sequents in HerbandBase in a run of the Chase.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}
module Chase.PossibleFacts.RelAlg.Lang 
    ( TableRef (..), Header, RelExp(..), Tup
    , Column, TupleD (..), Tuple (..), TuplePair (..), TupleSub (..)
    , TableD, Table, TablePair, TableSub, Database (..)
    , tuple, undecorate, insertIntoTable
    , emptyTableD, fullTableD, nullTableD, fullTableHeader
    , emptyTable, fullTable, nullTable
    , decorateTable, undecorateTable
    , emptyTablePair, nullTablePair
    , emptyTableSub, fullTableSub, nullTableSub
    , emptyDatabase, nullDatabase, emptyDatabaseWithConstants
    , tupleFromList, tuplePairFromList, recordsFromList, recordPairsFromList
    , tableFromList, tablePairFromList, databaseFromList
    , nubTable, unionTables, unionsTables, unionDatabases
    , diffTables, diffDatabases, removeEmptyTables
    , relExpRefs, refsInDatabase, databaseSize
    , deleteColumnProjector, similarColumnsSelector, columnValuesSelector
    , mergeJoinTables ) where

import Chase.PossibleFacts.RelAlg.ILang
