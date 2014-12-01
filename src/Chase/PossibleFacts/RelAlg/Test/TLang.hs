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

  Module      : Chase.PossibleFacts.RelAlg.Test.TLang
  Description : Unit tests for Translate
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Chase.PossibleFacts.RelAlg.Test.TLang where

-- HUnit
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Syntax
import Syntax.Term (Constant (..))

-- Test
import Chase.PossibleFacts.RelAlg.Test.TestData

-- RelAlg
import Chase.PossibleFacts.RelAlg.ILang

test_relExpRefs =
    [ "test relExpRefs for TblEmpty" ~: [] ~=? relExpRefs TblEmpty
    , "test relExpRefs for TblFull"  ~: [] ~=? relExpRefs TblFull
    , "test relExpRefs for atomic formula"  ~: 
      [ tblRef_P ] ~=? relExpRefs exp_Px
    , "test relExpRefs for atomic formula with constants"  ~: 
      [ tblRef_Q, tblRef_a, tblRef_b ] ~=? relExpRefs exp_Qab
    , "test relExpRefs for atomic formula with constant and variable"  ~: 
      [ tblRef_Q, tblRef_a ] ~=? relExpRefs exp_Qxa
    , "test relExpRefs for formula with duplicate variables" ~:
      [ tblRef_P ] ~=? relExpRefs exp_Pxx
    , "test relExpRefs for formula with duplicate variables and constant" ~:
      [ tblRef_P, tblRef_c ] ~=? relExpRefs exp_Pxcx
    , "test relExpRefs for atomic formula with projection"  ~: 
      [ tblRef_P ] ~=? relExpRefs exp_existsPx
    , "test relExpRefs for atomic formula with join"  ~: 
      [ tblRef_P, tblRef_Q ] ~=? relExpRefs exp_PxQy
    , "test relExpRefs for atomic formula with multiple joins"  ~: 
      [ tblRef_P, tblRef_Q, tblRef_R ] ~=? relExpRefs exp_PxQxRx
    , "test relExpRefs for delta"  ~: 
      [ tblRef_R ] ~=? relExpRefs exp_dRxy
    , "test relExpRefs for delta with projection"  ~: 
      [ tblRef_R ] ~=? relExpRefs exp_dexistsRxy0
    , "test relExpRefs for atomic formula with union"  ~: 
      [ tblRef_P, tblRef_Q ] ~=? relExpRefs exp_PxyzQyw
    ]

test_refsInDatabase = 
    [ "test refsInDatabase for empty set of references"  ~: 
      False ~=? refsInDatabase [] db_db1
    , "test refsInDatabase for empty database"  ~: 
      False ~=? refsInDatabase [ tblRef_P ] db_empty
    , "test refsInDatabase for non-existing references"  ~: 
      False ~=? refsInDatabase [ tblRef_R, tblRef_a ] db_db1
    , "test refsInDatabase for existing reference"  ~: 
      True  ~=? refsInDatabase [ tblRef_R, tblRef_P ] db_db1
    , "test refsInDatabase for existing constant reference"  ~: 
      True  ~=? refsInDatabase [ tblRef_R, tblRef_b ] db_afterDB14
    ]

test_unionTables =
    [ "test unionTables for empty tabels"  ~: 
      (tableFromList []) ~=? unionTables (tableFromList []) (tableFromList [])
    , "test unionTables for empty tabel on left"  ~: 
      (set_PinDB1) ~=? unionTables (tableFromList []) (set_PinDB1)
    , "test unionTables for empty tabel on right"  ~: 
      (set_PinDB1) ~=? unionTables (set_PinDB1) (tableFromList [])
    , "test unionTables for unioning a table with itself"  ~: 
      (set_QinDB1) ~=? unionTables (set_QinDB1) (set_QinDB1)
    , "test unionTables for tables without tuples in common"  ~: 
      (set_aunionbinDB16) ~=? unionTables (set_ainDB16) (set_binDB16)
    , "test unionTables for tables with tuples in common"  ~: 
      (set_QunionTinDB3) ~=? unionTables (set_QinDB3) (set_TinDB3)
    ]

test_unionDatabases =
    [ "test unionDatabase for empty databases"  ~: 
      db_empty ~=? unionDatabases db_empty db_empty
    , "test unionDatabase for empty database on left"  ~: 
      db_db1 ~=? unionDatabases db_empty db_db1
    , "test unionDatabase for empty database on right"  ~: 
      db_db1 ~=? unionDatabases db_db1 db_empty
    , "test unionDatabases for unioning a database with itself"  ~: 
      (db_db2) ~=? unionDatabases (db_db2) (db_db2)
    , "test unionDatabases without tables in common"  ~: 
      (db_db17_1union2) ~=? unionDatabases db_db17_1 db_db17_2
    , "test unionDatabases with tables in common"  ~: 
      (db_db18_1union2) ~=? unionDatabases db_db18_1 db_db18_2
    ]

test_diffTables = 
    [ "test diffTables for empty tables" ~:
      emptyTable ~=? diffTables emptyTable emptyTable
    , "test diffTables for empty table on left" ~:
      emptyTable ~=? diffTables emptyTable set_QinDB1
    , "test diffTables for empty table on right" ~:
      set_QinDB1 ~=? diffTables set_QinDB1 emptyTable
    , "test diffTables for tables without tuples in common" ~:
      set_ainDB16 ~=? diffTables set_ainDB16 set_binDB16
    , "test diffTables for tables with tuples in common" ~:
      set_QdiffTinDB3 ~=? diffTables set_QinDB3 set_TinDB3
    , "test diffTables for identical tables" ~:
      emptyTable ~=? diffTables set_QinDB3 set_QinDB3
    ]

test_diffDatabases = 
    [ "test diffDatabases for empty databases" ~:
      emptyDatabase ~=? diffDatabases emptyDatabase emptyDatabase
    , "test diffDatabases for empty database on left" ~:
      db_db1 ~=? diffDatabases db_db1 emptyDatabase
    , "test diffDatabases for empty database on right" ~:
      emptyDatabase ~=? diffDatabases emptyDatabase db_db1
    , "test diffDatabases for databases without tables in common" ~:
      db_db17_1 ~=? diffDatabases db_db17_1 db_db17_2
    , "test diffDatabases for databases with tables in common" ~:
      db_db18_1diff2 ~=? diffDatabases db_db18_1 db_db18_2
    , "test diffDatabases for identical databases" ~:
      emptyDatabase ~=? diffDatabases db_db1 db_db1
    ]

test_removeEmptyTables =
    [ "test removeEmptyTables for emptyDatabase" ~:
      emptyDatabase ~=? removeEmptyTables emptyDatabase
    , "test removeEmptyTables for database with no empty tables" ~:
      db_db19_2 ~=? removeEmptyTables db_db19_1
    , "test removeEmptyTables for database with only empty tables" ~:
      emptyDatabase ~=? removeEmptyTables db_db19_3
    ]

test_emptyDatabaseWithConstants =
    [ "test emptyDatabaseWithConstants for empty list of constants" ~:
      emptyDatabase ~=? emptyDatabaseWithConstants []
    , "test emptyDatabaseWithConstants for one constant" ~:
      db_emptyWith1 ~=? emptyDatabaseWithConstants [ Constant "a" ]
    , "test emptyDatabaseWithConstants for multiple constants" ~:
      db_emptyWith2 ~=? 
                    emptyDatabaseWithConstants [ Constant "a", Constant "b" 
                                               , Constant "c"]
    ]

test_all = test_relExpRefs ++ test_refsInDatabase 
           ++ test_unionTables ++ test_unionDatabases
           ++ test_diffTables ++ test_diffDatabases
           ++ test_removeEmptyTables
           ++ test_emptyDatabaseWithConstants