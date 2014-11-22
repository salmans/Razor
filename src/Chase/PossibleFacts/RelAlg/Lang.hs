{-| 
  Razor
  Module      : Chase.PossibleFacts.RelAlg.Lang
  Description : Provides the language of relational algebra for evaluating 
  sequents in HerbandBase in a run of the Chase.
  Maintainer  : Salman Saghafi
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
