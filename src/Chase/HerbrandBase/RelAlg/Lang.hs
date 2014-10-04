{-| 
  Razor
  Module      : Chase.HerbrandBase.RelAlg.Lang
  Description : Provides the language of relational algebra for evaluating 
  sequents in HerbandBase in a run of the Chase.
  Maintainer  : Salman Saghafi
-}
module Chase.HerbrandBase.RelAlg.Lang 
    ( TableRef (..), Header, RelExp(..), Tup
    , Column, TupleD (..), Tuple (..), TuplePair (..), TupleSub (..)
    , TableD, Table, TablePair, TableSub, Database (..)
    , tuple, undecorate
    , emptyTableD, fullTableD, nullTableD, fullTableHeader
    , emptyTable, fullTable, nullTable
    , decorateTable, undecorateTable
    , emptyTablePair, nullTablePair
    , emptyTableSub, fullTableSub, nullTableSub
    , emptyDatabase, nullDatabase, emptyDatabaseWithConstants
    , tupleFromList, tuplePairFromList, recordsFromList, recordPairsFromList
    , tableFromList, tablePairFromList, databaseFromList
    , tableTuples
    , unionTables, unionDatabases
    , diffTables, diffDatabases, removeEmptyTables
    , relExpRefs, refsInDatabase, databaseSize
    , deleteColumnProjector, deleteColumnsProjector
    , similarColumnsSelector, columnValuesSelector
    , mergeJoinTables ) where

import Chase.HerbrandBase.RelAlg.ILang
