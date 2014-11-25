{- Razor
   Module      : Chase.PossibleFacts.RelAlg.Test.TTranslate
   Description : Unit tests for Translate
   Maintainer  : Salman Saghafi -}

module Chase.PossibleFacts.RelAlg.Test.TTranslate where

-- Standard
import qualified Data.Map as Map

-- HUnit
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Test
import Syntax.Test.TestData -- import test data for formulas
import Chase.PossibleFacts.RelAlg.Test.TestData

-- Common
import Common.Provenance

-- SAT
import SAT.Data

-- Syntax
import Syntax.Term (Variable (..), Term (..))

-- Chase
import Chase.Data (PushM, evalPushM, evalPullM)

-- RelAlg
import Chase.PossibleFacts.RelAlg.Lang
import qualified Chase.PossibleFacts.RelAlg.DB as DB
import Chase.PossibleFacts.RelAlg.ITranslate
import Chase.PossibleFacts.RelAlg.PossibleFacts

-- Tools
import Tools.Config (Config (..), defaultConfig)


defaultDepth = configDefaultSkolemDepth defaultConfig


test_createInternalVarMap = 
    [ "test createInternalVarMap for an empty list of variables" ~: 
      Map.empty  ~=? createInternalVarMap []
    , "test createInternalVarMap for a list of variable terms" ~:
      Map.fromList [(var "x", [0]), (var "y", [1])] ~=? 
      createInternalVarMap [Var $ var "x", Var $ var "y"]
    , "test createInternalVarMap for a list of variable and constant terms" ~:
      Map.fromList [(var "x", [0]), (var "y", [2])] ~=? 
      createInternalVarMap [Var $ var "x", Cons $ cons "c", Var $ var "y"]
    , "test createInternalVarMap for a list with duplicate variables" ~:
      Map.fromList [(var "x", [0, 2]), (var "y", [1])] ~=? 
      createInternalVarMap [Var $ var "x", Var $ var "y", Var $ var "x"]
    , "test createInternalVarMap for a complex list" ~:
      Map.fromList [(var "x", [0, 3]), (var "y", [1, 5, 6])] ~=? 
      createInternalVarMap [ Var $ var "x", Var $ var "y", Cons $ cons "c"
                   , Var $ var "x", Cons $ cons "d", Var $ var "y"
                   , Var $ var "y"]
    ]

test_createExternalVarMap = 
    [ "test createExternalVarMap for an empty list of variables" ~: 
      Map.empty  ~=? createExternalVarMap []
    , "test createExternalVarMap for a list of variable terms" ~:
      Map.fromList [(var "x", [0]), (var "y", [1])] ~=? 
      createExternalVarMap [Var $ var "x", Var $ var "y"]
    , "test createExternalVarMap for a list of variable and constant terms" ~:
      Map.fromList [(var "x", [0]), (var "y", [1])] ~=? 
      createExternalVarMap [Var $ var "x", Cons $ cons "c", Var $ var "y"]
    , "test createExternalVarMap for a list with duplicate variables" ~:
      Map.fromList [(var "x", [0, 2]), (var "y", [1])] ~=? 
      createExternalVarMap [Var $ var "x", Var $ var "y", Var $ var "x"]
    , "test createExternalVarMap for a complex list" ~:
      Map.fromList [(var "x", [0, 2]), (var "y", [1, 3, 4])] ~=? 
      createExternalVarMap [ Var $ var "x", Var $ var "y", Cons $ cons "c"
                   , Var $ var "x", Cons $ cons "d", Var $ var "y"
                   , Var $ var "y"]
    ]

test_equalVarPairs = 
    [ "test equalVarPairs for an empty map" ~: 
      [] ~=? equalVarPairs Map.empty (heads [])
    , "test equalVarPairs for a map with no equal pairs" ~: 
      [(0,0), (2,1)] ~=? 
      (equalVarPairs (Map.fromList [(var "x", [0]), (var "y", [2])])
                         $ heads [var "x", var "y"])
    , "test equalVarPairs for a list with duplicate variables" ~:
      [(0,0), (2, 0), (1,1)] ~=? 
      (equalVarPairs (Map.fromList [(var "x", [0, 2]), (var "y", [1])])
                         $ heads [var "x", var "y"])
    , "test createVarMap for a complex variable map" ~:
      [(0,0), (3, 0), (1,1), (5,1), (6, 1)] ~=? 
      (equalVarPairs (Map.fromList [(var "x", [0, 3]), (var "y", [1, 5, 6])])
                         $ heads [var "x", var "y"])
    ]

test_varJoinPairs = 
    [ "test varJoinPairs for an empty header on left" ~: 
      [] ~=? varJoinPairs (heads []) (heads [var "x", var "y"])
    , "test varJoinPairs for an empty header on right" ~: 
      [] ~=? varJoinPairs (heads [var "x", var "y"]) (heads [])
    -- , "test varJoinPairs for mismatching headers" ~: 
    --   [(1, 0)] ~=? varJoinPairs (heads [var "x", var "y"]) 
    --                             (heads [var "y", var "z"])
    -- This should raise an error
    , "test varJoinPairs for matching headers" ~: 
      [(0, 1), (1, 2)] ~=? 
      varJoinPairs (heads [var "x", var "w", var "y"]) 
                   (heads [var "w", var "y"])
    ]



test_headRelExp =
     [ "test headRelExp for Truth" ~:
       [TblFull] ~=? (headRelExp fmla_truth)

     , "test headRelExp for an atomic formula" ~:
       [exp_Px] ~=? (headRelExp fmla_Px)
     , "test headRelExp for an atomic formula with constant" ~:
       [exp_Rxc] ~=? (headRelExp fmla_Rxc)
     , "test headRelExp for an atomic formula with repetitive variables" ~:
       [exp_Pxx] ~=? (headRelExp fmla_Pxx)
     , "test headRelExp for a complex atomic formula" ~:
       [exp_Pxcx] ~=? (headRelExp fmla_Pxcx)

     , "test headRelExp for conjunction without shared variables" ~:
       [exp_PxQy] ~=? headRelExp fmla_PxAndQy
     , "test headRelExp for conjunction with shared variables" ~:
       [exp_PxQx] ~=? (headRelExp fmla_PxAndQx)
     , "test headRelExp for conjunction with different arities" ~:
       [exp_PxQxy] ~=? (headRelExp fmla_PxAndQxy)

     , "test headRelExp for two conjunctions" ~:
       [exp_PxQxRx] ~=? headRelExp fmla_PxAndQxAndRx

     , "test headRelExp for disjunction" ~:
       [ exp_Px, exp_Qy ] ~=? headRelExp fmla_PxOrQy

     , "test headRelExp for existentials" ~:
       [ exp_existsPx ] ~=? headRelExp fmla_existsPx_R
     , "test headRelExp for existentials with free variables" ~:
       [ exp_fmlaExists1 ] ~=? headRelExp fmla_exists1_R
     , "test headRelExp for testing shifting in left headers" ~:
       [ exp_PaxRxy ] ~=? headRelExp fmla_PaxAndRxy
     , "test headRelExp for testing shifting in right headers" ~:
       [ exp_PyQxRxy ] ~=? headRelExp fmla_PyAndQxAndRxy
     ]

test_bodyRelExp =
    [ "test bodyRelExp for Falsehood" ~:
      TblEmpty ~=? (bodyRelExp fmla_falsehood)

     , "test bodyRelExp for an atomic formula" ~:
      exp_Px ~=? (bodyRelExp fmla_Px)
     , "test bodyRelExp for an atomic formula with constant" ~:
      exp_Rxc ~=? (bodyRelExp fmla_Rxc)
     , "test bodyRelExp for an atomic formula with repetitive variables" ~:
      exp_Pxx ~=? (bodyRelExp fmla_Pxx)
     , "test bodyRelExp for a complex atomic formula" ~:
      exp_Pxcx ~=? (bodyRelExp fmla_Pxcx)

     , "test bodyRelExp for conjunction without shared variables" ~:
      exp_PxQy ~=? bodyRelExp fmla_PxAndQy
     , "test bodyRelExp for conjunction with shared variables" ~:
      exp_PxQx ~=? (bodyRelExp fmla_PxAndQx)
     , "test bodyRelExp for two conjunctions" ~:
      exp_PxQxRx ~=? bodyRelExp fmla_PxAndQxAndRx
     , "test bodyRelExp for testing shifting in left headers" ~:
      exp_PaxRxy ~=? bodyRelExp fmla_PaxAndRxy
     , "test bodyRelExp for testing shifting in right headers" ~:
      exp_PyQxRxy ~=? bodyRelExp fmla_PyAndQxAndRxy
    ]

test_evaluateRelExp =
    [ "test evaluateRelExp for TblEmpty" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db1 db_empty TblEmpty)
     
    , "test evaluateRelExp for TblFull" ~:
      fullTable ~=? 
      unextendTable (evaluateRelExp db_db1 db_empty TblFull)

    , "test evaluateRelExp in an empty database" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_empty db_empty exp_Rx)
    , "test evaluateRelExp for a table" ~:
      tableFromList [[elm "a"], [elm "b"]] ~=? 
      unextendTable (evaluateRelExp db_db1 db_empty exp_Px)
    , "test evaluateRelExp for a full table with only constants" ~:
      fullTable ~=? 
      unextendTable (evaluateRelExp db_db2 db_empty exp_Qab)
    , "test evaluateRelExp for a non-existing record with only constants" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db16 db_empty exp_Pbc)
    , "test evaluateRelExp for a empty table with only constants" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db3 db_empty exp_Qab)
    , "test evaluateRelExp for a table with constant and variable" ~:
      tableFromList [ [elm "a"], [elm "b"] ] ~=? 
      unextendTable (evaluateRelExp db_db2 db_empty exp_Qxb)
    , "test evaluateRelExp for a table with non-existing constant" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db2 db_empty exp_Qxc)
    , "test evaluateRelExp for a table with no returning records" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db2 db_empty exp_Qxa)
    , "test evaluateRelExp for a non-existing table" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db1 db_empty exp_Rx)

    , "test evaluateRelExp for delta in an empty database" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_empty db_empty exp_dRx)
    , "test evaluateRelExp for delta of a table" ~:
      tableFromList [[elm "a"], [elm "b"]] ~=? 
      unextendTable (evaluateRelExp db_empty db_db1 exp_dPx)
    , "test evaluateRelExp for delta of a non-existing table" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db1 db_db1 exp_dRx)

    , "test evaluateRelExp projecting empty table" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db1 db_empty exp_existsRxy0)
    , "test evaluateRelExp projecting" ~:
      tableFromList [[elm "a"], [elm "b"]] ~=? 
      unextendTable (evaluateRelExp db_db1 db_empty exp_existsQxy1)

    , "test evaluateRelExp projecting all columns out from an empty set" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db1 db_empty exp_existsRx)
    , "test evaluateRelExp projecting all columns out from a full set" ~:
      fullTable ~=? 
      unextendTable (evaluateRelExp db_db1 db_empty exp_existsPx)

    -- test too large indices

    , "test evaluateRelExp selecting from an empty table" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db1 db_empty exp_selRx)
    , "test evaluateRelExp selecting based on similar variable indices" ~:
      tableFromList [[elm "c", elm "b", elm "c"]] ~=? 
      unextendTable (evaluateRelExp db_db2 db_empty exp_selPxyx)

    , "test evaluateRelExp for join" ~:
      set_PjoinQinDB2 ~=? 
      unextendTable (evaluateRelExp db_db2 db_empty exp_PxyzQyw)
    , "test evaluateRelExp for join with empty set on left" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db2 db_empty exp_RxyQyz)
    , "test evaluateRelExp for join with empty set on right" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db2 db_empty exp_PxyzRy)
    -- select also needs to deal with large indices just like the case for proj

    , "test evaluateRelExp for union with empty delta" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db2 db_empty exp_uniPxyzQyw)

    , "test evaluateRelExp for union with empty table on left" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db3 db_delta3 exp_uniRxyQyz)
    , "test evaluateRelExp for union with empty table on right" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db3 db_delta3 exp_uniPxyzRy)
    , "test evaluateRelExp for union" ~:
      set_DeltaPjoinQinDB3AndDelta3 ~=? 
      unextendTable (evaluateRelExp db_db3 db_delta3 exp_uniPxyzQyw)
    , "test evaluateRelExp for union for non-changing sets" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExp db_db3 db_delta3 exp_uniSxTxy)
    , "test evaluateRelExp for union with values only in delta" ~:
      set_DeltaPjoinQinDB20AndDelta20 ~=? 
      unextendTable (evaluateRelExp db_db20 db_delta20 exp_uniPxyzQyw)
    -- property: union and join in original database is equal to the join
    -- in the final database
    ]

test_evaluateRelExpNoDelta =
    [ "test evaluateRelExpNoDelta for TblEmpty" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db1 TblEmpty)

    , "test evaluateRelExpNoDelta for TblFull" ~:
      fullTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db1 TblFull)

    , "test evaluateRelExpNoDelta in an empty database" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_empty exp_Rx)
    , "test evaluateRelExpNoDelta for a table" ~:
      tableFromList [[elm "a"], [elm "b"]] ~=? 
      unextendTable (evaluateRelExpNoDelta db_db1 exp_Px)
    , "test evaluateRelExpNoDelta for a full table with only constants" ~:
      fullTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db2 exp_Qab)
    , "test evaluateRelExpNoDelta for non-existing record with only constants" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db16 exp_Pbc)
    , "test evaluateRelExpNoDelta for a empty table with only constants" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db3 exp_Qab)
    , "test evaluateRelExpNoDelta for a table with constant and variable" ~:
      tableFromList [ [elm "a"], [elm "b"] ] ~=? 
      unextendTable (evaluateRelExpNoDelta db_db2 exp_Qxb)
    , "test evaluateRelExpNoDelta for a table with non-existing constant" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db2 exp_Qxc)
    , "test evaluateRelExpNoDelta for a table with no returning records" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db2 exp_Qxa)
    , "test evaluateRelExpNoDelta for a non-existing table" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db1 exp_Rx)

    , "test evaluateRelExpNoDelta for delta in an empty database" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_empty exp_dRx)
    , "test evaluateRelExpNoDelta for delta of a table" ~:
      tableFromList [[elm "a"], [elm "b"]] ~=? 
      unextendTable (evaluateRelExpNoDelta db_db1 exp_dPx)
    , "test evaluateRelExpNoDelta for delta of a non-existing table" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db1 exp_dRx)

    , "test evaluateRelExpNoDelta projecting empty table" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db1 exp_existsRxy0)
    , "test evaluateRelExpNoDelta projecting" ~:
      tableFromList [[elm "a"], [elm "b"]] ~=? 
      unextendTable (evaluateRelExpNoDelta db_db1 exp_existsQxy1)

    , "test evaluateRelExpNoDelta projecting all columns out of an empty set" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db1 exp_existsRx)
    , "test evaluateRelExpNoDelta projecting all columns out of a full set" ~:
      fullTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db1 exp_existsPx)

    -- test too large indices

    , "test evaluateRelExpNoDelta selecting from an empty table" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db1 exp_selRx)
    , "test evaluateRelExpNoDelta selecting based on similar variable indices" ~:
      tableFromList [[elm "c", elm "b", elm "c"]] ~=? 
      unextendTable (evaluateRelExpNoDelta db_db2 exp_selPxyx)

    , "test evaluateRelExpNoDelta for join" ~:
      set_PjoinQinDB2 ~=? 
      unextendTable (evaluateRelExpNoDelta db_db2 exp_PxyzQyw)
    , "test evaluateRelExpNoDelta for join with empty set on left" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db2 exp_RxyQyz)
    , "test evaluateRelExpNoDelta for join with empty set on right" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db2 exp_PxyzRy)
    -- select also needs to deal with large indices just like the case for proj

    , "test evaluateRelExpNoDelta for union with empty database" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db20 exp_uniPxyzQyw)

    , "test evaluateRelExpNoDelta for union with empty table on left" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db3 exp_uniRxyQyz)
    , "test evaluateRelExpNoDelta for union with empty table on right" ~:
      emptyTable ~=? 
      unextendTable (evaluateRelExpNoDelta db_db3 exp_uniPxyzRy)
    , "test evaluateRelExpNoDelta for union" ~:
      set_PjoinQinDB3 ~=? 
      unextendTable (evaluateRelExpNoDelta db_db3 exp_uniPxyzQyw)
    -- property: union and join in original database is equal to the join
    -- in the final database
    ]

test_delta =
    [ "test delta for TblEmpty" ~:
      TblEmpty ~=? (delta TblEmpty)
    , "test delta for TblFull" ~:
      TblFull ~=? (delta TblFull)
    , "test delta for atomic formula" ~:
      exp_dPx ~=? (delta exp_Px)
    , "test delta for atomic formula with constant" ~:
      exp_dQxc ~=? (delta exp_Qxc)
    , "test delta for atomic formula with constant and duplicate variables" ~:
      exp_dPxcx ~=? (delta exp_Pxcx)
    , "test delta for formula with projection" ~:
      exp_dexistsRxy0 ~=? (delta exp_existsRxy0)
    , "test delta for formula with join" ~:
      exp_dPxyzQyw ~=? (delta exp_PxyzQyw)
    ]

test_insertTuples =
    [ -- "test insertTuple TblEmpty" ~:
      -- db_empty ~=? (insertTuples set_PinDB2 TblEmpty db_empty)
      -- This is supposed to rais an exception
      "test insertTuple for TblFull in an empty database" ~:
      db_empty ~=? 
      evalPushM (insertTuples setPair_PinsertDB7 TblFull db_empty defaultDepth) 
                db_empty emptyProvInfo 0 emptyPropTheory defaultConfig
    , "test insertTuple for TblFull in a non-empty database" ~:
      db_db1 ~=? 
      evalPushM (insertTuples setPair_PinsertDB7 TblFull db_db1 defaultDepth) 
                db_empty emptyProvInfo 0 emptyPropTheory defaultConfig

    , "test insertTuple for an empty tuple set" ~:
     db_db1 ~=? 
      evalPushM (insertTuples emptyTablePair exp_Pxy db_db1 defaultDepth) 
                db_empty emptyProvInfo 0 emptyPropTheory defaultConfig

    , "test insertTuple for a single tuple in an empty database" ~:
     db_db4 ~=? 
      evalPushM (insertTuples setPair_PinDB4 exp_Px db_empty defaultDepth) 
                db_empty emptyProvInfo 0 emptyPropTheory defaultConfig
    , "test insertTuple for multiple tuples in an empty dabase" ~:
     db_db5 ~=? 
      evalPushM (insertTuples setPair_QinDB5 exp_Qxy db_empty defaultDepth) 
                db_empty emptyProvInfo 0 emptyPropTheory defaultConfig
    , "test insertTuple for multiple tuples in a non-empty database" ~:
     db_db6 ~=? 
      evalPushM (insertTuples setPair_QinDB5 exp_Qxy db_db4 defaultDepth) 
                db_empty emptyProvInfo 0 emptyPropTheory defaultConfig
    , "test insertTuple for inserting into a constant table" ~:
     db_afterDB14 ~=? 
      evalPushM (insertTuples setPair_QinsertDB14 exp_Qxb 
                              db_beforeDB14 defaultDepth) 
                db_empty emptyProvInfo 0 emptyPropTheory defaultConfig
    , "test insertTuple for constant with duplicate variables" ~:
     db_afterDB15 ~=? 
      evalPushM (insertTuples setPair_PinsertDB15 exp_Pxcx 
                              db_beforeDB15 defaultDepth) 
                db_empty emptyProvInfo 0 emptyPropTheory defaultConfig
    , "test insertTuple for adding to an existing table" ~:
     db_afterDB7 ~=? 
     evalPushM (insertTuples setPair_PinsertDB7 exp_Pxy 
                             db_beforeDB7 defaultDepth) 
               db_empty emptyProvInfo 0 emptyPropTheory defaultConfig
    , "test insertTuple for existing tuples in the new database" ~:
     db_afterDB7_1 ~=? 
     evalPushM (insertTuples setPair_PinsertDB7_1 exp_Pxy 
                             db_beforeDB7_1 defaultDepth) 
               db_empty emptyProvInfo 0 emptyPropTheory defaultConfig
    , "test insertTuple for existing tuples in the old database" ~:
     db_newDB7_1 ~=? 
     evalPushM (insertTuples setPair_PinsertDB7_1 exp_Pxy 
                             db_empty defaultDepth) 
               db_beforeDB7_1 emptyProvInfo 0 emptyPropTheory defaultConfig

    , "test insertTuple for inserting into the head of projected expression" ~:
     db_afterDB8 ~=? 
     evalPushM (insertTuples setPair_PinsertDB8 exp_existsPxy0 
                             db_beforeDB8 defaultDepth) 
               db_empty emptyProvInfo 0 emptyPropTheory defaultConfig
    , "test insertTuple for inserting into a projected expression" ~:
     db_afterDB9 ~=? 
     evalPushM (insertTuples setPair_PinsertDB9 exp_existsPxy1 
                             db_beforeDB9 defaultDepth) 
               db_empty emptyProvInfo 0 emptyPropTheory defaultConfig
    , "test insertTuple projecting a unary table" ~:
     db_afterDB10 ~=? 
     evalPushM (insertTuples setPair_PinsertDB10 exp_existsPx 
                             db_beforeDB10 defaultDepth) 
               db_empty emptyProvInfo 0 emptyPropTheory defaultConfig
    , "test insertTuple duplicates into new database for projected expression" ~:
     db_afterDB11 ~=? 
     evalPushM (insertTuples setPair_PinsertDB11 exp_existsPxy0 
                             db_beforeDB11 defaultDepth) 
               db_empty emptyProvInfo 0 emptyPropTheory defaultConfig
    , "test insertTuple duplicates into old database for projected expression" ~:
     db_newDB11 ~=? 
     evalPushM (insertTuples setPair_PinsertDB11 exp_existsPxy0 
                             db_empty defaultDepth) 
               db_beforeDB11 emptyProvInfo 0 emptyPropTheory defaultConfig

--     -- test too large indices

    , "test insertTuple for Select with variable indices" ~:
     db_afterDB7_2 ~=? 
     evalPushM (insertTuples setPair_PinsertDB7_2 exp_Pxx 
                             db_beforeDB7_2 defaultDepth) 
               db_empty emptyProvInfo 0 emptyPropTheory defaultConfig

    -- property: inserting into a Select expression is like inserting into the
    -- original expression

    , "test insertTuple for join" ~:
     db_afterDB12 ~=? 
     evalPushM (insertTuples setPair_joinInsertDB12  exp_PxyzQyw 
                             db_beforeDB12 defaultDepth) 
     db_empty emptyProvInfo 0 emptyPropTheory defaultConfig
    , "test insertTuple for join with multiple indices" ~:
     db_afterDB13 ~=? 
     evalPushM (insertTuples setPair_joinInsertDB13 exp_PxyzQyz 
                             db_beforeDB13 defaultDepth) 
     db_empty emptyProvInfo 0 emptyPropTheory defaultConfig

    -- join with constant tables
    -- throws exception for Delt and Union
    ]

-- test_diffRelExpSets =
--     [ -- "test diffRelExpSets for TblFull on left with free variable on right" ~:
--       -- ~=? (diffRelExpSets db_db16 db_empty TblFull exp_Qxb id)
--       -- This should throw an exception

--       "test diffRelExpSets for TblFull on left and full table on right" ~:
--       setPair_emptyEmpty ~=? evalPullM 
--                         (diffRelExpSets db_db16 db_empty TblFull exp_Qab id)
--                         db_db16

--     , "test diffRelExpSets for TblFull on left and empty table on right" ~:
--       setPair_emptyFull ~=? evalPullM 
--                        (diffRelExpSets db_db16 db_empty TblFull exp_Pbc id)
--                        db_db16

--       -- , "test diffRelExpSets for TblFull on right" ~:
--       --    ~=? (diffRelExpSets db_db1 db_empty exp_Pxy TblFull id)
--       -- This should throw an exception
--       -- , "test diffRelExpSets for TblEmpty on left" ~:
--       --    ~=? (diffRelExpSets db_db1 db_empty TblEmpty exp_Pxy id)
--       -- This should throw an exception
--       -- , "test diffRelExpSets for TblEmpty on right" ~:
--       --    ~=? (diffRelExpSets db_db1 db_empty exp_Pxy TblEmpty id)
--       -- This should throw an exception

--     , "test diffRelExpSets for tables with similar headers" ~:
--       setPair_PxydiffQxy ~=? 
--       let transform = tupleTransformer (header exp_Pxy) (header exp_Qxy)
--       in  evalPullM (diffRelExpSets db_db16 db_empty exp_Pxy exp_Qxy transform) 
--           db_db16
--     , "test diffRelExpSets for tables with different attribute orders" ~:
--       setPair_PxydiffQyx ~=? 
--       let transform = tupleTransformer (header exp_Pxy) (header exp_Qyx)
--       in evalPullM (diffRelExpSets db_db16 db_empty exp_Pxy exp_Qyx transform) 
--          db_db16

--     -- , "test diffRelExpSets for extra variable on right" ~:
--     --  ~=? (diffRelExpSets db_db16 db_empty exp_Px exp_Qxy)
--     -- This should throw an exception

--     , "test diffRelExpSets for extra attribute on left and different order" ~:
--       setPair_PxyQyzdiffQyx ~=? 
--       let transform = tupleTransformer (header exp_PxyQyz) (header exp_Qyx) 
--       in evalPullM (diffRelExpSets db_db16 db_empty exp_PxyQyz exp_Qyx transform) 
--          db_db16
--     ]


test_all = test_createInternalVarMap ++ test_createExternalVarMap 
           ++ test_equalVarPairs ++ test_varJoinPairs
           ++ test_headRelExp ++ test_bodyRelExp ++ test_delta
           ++ test_evaluateRelExp ++ test_evaluateRelExpNoDelta
           ++ test_insertTuples -- ++ test_diffRelExpSets