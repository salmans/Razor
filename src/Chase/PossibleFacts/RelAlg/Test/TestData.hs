{- Razor
   Module      : Chase.PossibleFacts.RelAlg.TestData
   Description : Test data for RelAlg
   Maintainer  : Salman Saghafi -}

module Chase.PossibleFacts.RelAlg.Test.TestData where

-- Standard
import qualified Data.Map as Map

-- Syntax
import Syntax.Term (Constant (..), Variable (..), Element (..))


-- Test
import Syntax.Test.TestData

-- RelAgl
import Chase.PossibleFacts.RelAlg.Lang
import Chase.PossibleFacts.RelAlg.Translate
import qualified Chase.PossibleFacts.RelAlg.DB as DB


-- shortcuts
heads :: [Variable] -> Header
heads vs = Map.fromList (zip vs [0..])

duplicateRecords rs = map (\r -> (r, r)) rs -- to convert a set to a setpair

tblRef_B = RelTable "B"
tblRef_E = RelTable "E"
tblRef_D = RelTable "D"
tblRef_P = RelTable "P"
tblRef_Q = RelTable "Q"
tblRef_R = RelTable "R"
tblRef_S = RelTable "S"
tblRef_T = RelTable "T"
tblRef_U = RelTable "U"
tblRef_Eq = RelTable "="
tblRef_Element = RelTable "@Element"
tblRef_a = ConstTable (cons "a")
tblRef_b = ConstTable (cons "b")
tblRef_c = ConstTable (cons "c")
fnTblRef_f = FnTable "f"
fnTblRef_g = FnTable "g"

tbl_P = Tbl tblRef_P
tbl_Q = Tbl tblRef_Q
tbl_R = Tbl tblRef_R
tbl_S = Tbl tblRef_S
tbl_T = Tbl tblRef_T

exp_Pbc = tbl_P [] [(0, tblRef_b), (1, tblRef_c)] $ heads []
exp_Px  = tbl_P [(0, 0)] [] $ heads [var "x"]
exp_Py  = tbl_P [(0, 0)] [] $ heads [var "y"]
exp_Pax  = tbl_P [(1, 0)] [(0, tblRef_a)] $ heads [var "x"]
exp_Pxy = tbl_P [(0,0), (1,1)] [] $ heads [var "x", var "y"]
exp_Pxyz = tbl_P [(0,0), (1,1), (2,2)] [] $ heads [var "x", var "y", var "z"]
exp_Pxyx = tbl_P [(0,0), (1,1), (2,0)] [] $ heads [var "x", var "y", var "x"]
exp_Pxx = Sel (tbl_P [(0,0), (1,0)] [] (heads [var "x"])) 
          [(0,0), (1,0)] $ heads [var "x"]
exp_Pxcx   = Sel (tbl_P [(0,0), (2,0)] [(1, tblRef_c)] $ heads [var "x"]) 
             [(0,0), (2,0)] $ heads [var "x"]
exp_Rx = tbl_R [(0,0)] [] $ heads [var "x"]
exp_Ry = tbl_R [(0,0)] [] $ heads [var "y"]
exp_Rxy = tbl_R [(0,0), (1,1)] [] $ heads [var "x", var "y"]
exp_Qab = tbl_Q [] [(0, tblRef_a), (1, tblRef_b)] $ heads []
exp_Qxa = tbl_Q [(0, 0)] [(1, tblRef_a)] $ heads [var "x"]
exp_Qxb = tbl_Q [(0, 0)] [(1, tblRef_b)] $ heads [var "x"]
exp_Qxc = tbl_Q [(0, 0)] [(1, tblRef_c)] $ heads [var "x"]
exp_Qx = tbl_Q [(0,0)] [] $ heads [var "x"]
exp_Qy = tbl_Q [(0,0)] [] $ heads [var "y"]
exp_Qxy = tbl_Q [(0,0), (1,1)] [] $ heads [var "x", var "y"]
exp_Qyx = tbl_Q [(0,0), (1,1)] [] $ heads [var "y", var "x"]
exp_Qyw = tbl_Q [(0,0), (1,1)] [] $ heads [var "y", var "w"]
exp_Qyz = tbl_Q [(0,0), (1,1)] [] $ heads [var "y", var "z"]
exp_Sx  = tbl_S [(0,0)] [] $ heads [var "x"]
exp_Txy = tbl_T [(0,0), (1,1)] [] $ heads [var "x", var "y"]
exp_dPx = Delta exp_Px $ heads [var "x"]
exp_dPxyz = Delta exp_Pxyz $ heads [var "x", var "y", var "z"]
exp_dPxcx = Sel (Delta (tbl_P [(0,0), (2,0)] [(1, tblRef_c)] $ heads [var "x"])
                 (heads [var "x"]))            
            [(0,0), (2,0)] $ heads [var "x"]
exp_dQxc = Delta exp_Qxc $ heads [var "x"]
exp_dRx = Delta exp_Rx $ heads [var "x"]
exp_dRy = Delta exp_Ry $ heads [var "y"]
exp_dQyz   = Delta exp_Qyz $ heads [var "y", var "z"]
exp_dQyw  = Delta exp_Qyw $ heads [var "y", var "w"]
exp_dRxy   = Delta exp_Rxy $ heads [var "x", var "y"]
exp_dSx    = Delta exp_Sx  $ heads [var "x"]
exp_dTxy   = Delta exp_Txy $ heads [var "x", var "y"]
exp_dexistsRxy0 = Proj exp_dRxy 0 (heads [var "y"]) "exists#0" Nothing
exp_dPxyzQyw = Union exp_Pxyz exp_dPxyz exp_Qyw exp_dQyw
               $ heads [var "x", var "y", var "z", var "w"]

exp_PxQy    = Join exp_Px exp_Qy $ heads [var "x", var "y"]
exp_PxQx    = Join exp_Px exp_Qx $ heads [var "x"]
exp_QxRx    = Join exp_Qx exp_Rx $ heads [var "x"]
exp_QxRxy   = Join exp_Qx exp_Rxy $ heads [var "x", var "y"]
exp_PxQxRx  = Join exp_Px exp_QxRx $ heads [var "x"]
exp_PaxRxy  = Join exp_Pax exp_Rxy $ heads [var "x", var "y"]
exp_PyQxRxy = Join exp_Py exp_QxRxy $ heads [var "y", var "x"]
exp_Rxc     = tbl_R [(0,0)] [(1, tblRef_c)] $ heads [var "x"]
exp_PxyzQyw = Join exp_Pxyz exp_Qyw
              $ heads [var "x", var "y", var "z", var "w"]
exp_PxyzQyz = Join exp_Pxyz exp_Qyz $ heads [var "x", var "y", var "z"]
exp_PxyzRy  = Join exp_Pxyz exp_Ry $ heads [var "x", var "y", var "z"]
exp_PxyQyz  = Join exp_Pxy exp_Qyz $ heads [var "x", var "y", var "z"]
exp_RxyQyz  = Join exp_Rxy exp_Qyz $ heads [var "x", var "y", var "z"]
exp_SxTxy   = Join exp_Sx exp_Txy  $ heads [var "x", var "y"]
exp_PxQxy   = Join exp_Px exp_Qxy  $ heads [var "x", var "y"]

exp_existsPx    = Proj exp_Px 0 Map.empty "exists#0" Nothing
exp_existsRx    = Proj exp_Rx 0 Map.empty "exists#0" Nothing
exp_existsRxy0  = Proj exp_Rxy 0 (heads [var "y"]) "exists#0" Nothing
exp_existsQxy1  = Proj exp_Qxy 1 Map.empty "exists#0" Nothing
exp_existsPxy0  = Proj exp_Pxy 0 (heads [var "y"]) "exists#0" Nothing
exp_existsPxy1  = Proj exp_Pxy 1 (heads [var "x"]) "exists#0" Nothing
exp_fmlaExists1 = Proj exp_Pxy 0 (heads [var "y"]) "exists#0" Nothing
exp_selRx       = Sel exp_Rx [] $ heads [var "x"]
exp_selPxyx     = Sel exp_Pxyx [(0, 2)] $ heads [var "x", var "y"]
exp_uniPxyzQyw  = Union exp_Pxyz exp_dPxyz exp_Qyw exp_dQyw 
                  $ heads [var "x", var "y", var "z", var "w"]
exp_uniRxyQyz   = Union exp_Rxy exp_dRxy exp_Qyz exp_dQyz 
                  $ heads [var "x", var "y", var "z"]
exp_uniPxyzRy   = Union exp_Pxyz exp_dPxyz exp_Ry exp_dRy 
                  $ heads [var "x", var "y", var "z"]
exp_uniSxTxy    = Union exp_Sx exp_dSx exp_Txy exp_dTxy 
                  $ heads [var "x", var "y"]


db_empty     = databaseFromList []

setPair_emptyEmpty = tablePairFromList $ duplicateRecords []


set_ainEmptyWith1       = tableFromList [ [elm "a"] ]
set_ElementinEmptyWith1 = tableFromList [ [elm "a"] ]
db_emptyWith1     = databaseFromList [ (tblRef_a, set_ainEmptyWith1)
                                     , (tblRef_Element
                                       , set_ElementinEmptyWith1) ]
set_ainEmptyWith2       = tableFromList [ [elm "a"] ]
set_binEmptyWith2       = tableFromList [ [elm "b"] ]
set_cinEmptyWith2       = tableFromList [ [elm "c"] ]
set_ElementinEmptyWith2 = tableFromList [ [elm "a"], [elm "b"], [elm "c"] ]
db_emptyWith2     = databaseFromList [ (tblRef_a, set_ainEmptyWith2)
                                     , (tblRef_b, set_binEmptyWith2)
                                     , (tblRef_c, set_cinEmptyWith2)
                                     , (tblRef_Element
                                       , set_ElementinEmptyWith2) ]


set_PinDB1   = tableFromList [ [elm "a"], [elm "b"] ]
set_QinDB1   = tableFromList [ [elm "a", elm "b"]
                             , [elm "b", elm "c"] ]
db_db1   = databaseFromList [(tblRef_P, set_PinDB1), (tblRef_Q, set_QinDB1)]

set_ainDB2   = tableFromList [ [elm "a"] ]
set_binDB2   = tableFromList [ [elm "b"] ]
set_PinDB2   = tableFromList [ [elm "a", elm "b", elm "c"] 
                             , [elm "a", elm "a", elm "c"] 
                             , [elm "c", elm "b", elm "c"] ]
set_QinDB2   = tableFromList [ [elm "a", elm "b"] 
                             , [elm "b", elm "b"] 
                             , [elm "b", elm "c"] ]
set_PjoinQinDB2 = 
    tableFromList [ [ elm "a", elm "b", elm "c", elm "b"]
                  , [ elm "a", elm "b", elm "c", elm "c"] 
                  , [ elm "a", elm "a", elm "c", elm "b"]
                  , [ elm "c", elm "b", elm "c", elm "b"] 
                  , [ elm "c", elm "b", elm "c", elm "c"] ]
db_db2   = databaseFromList [ (tblRef_a, set_ainDB2), (tblRef_b, set_binDB2)
                            , (tblRef_P, set_PinDB2), (tblRef_Q, set_QinDB2)]

set_PinDB3   = tableFromList [ [elm "c", elm "b", elm "c"] ]
set_QinDB3   = tableFromList [ [elm "b", elm "b"] 
                             , [elm "b", elm "c"] ]
set_QunionTinDB3 = tableFromList [ [elm "b", elm "b"] 
                                 , [elm "b", elm "c"] 
                                 , [elm "a", elm "a"] ]
set_SinDB3   = tableFromList [ [elm "a"] 
                             , [elm "b"] ]
set_TinDB3   = tableFromList [ [elm "a", elm "a"] 
                             , [elm "b", elm "b"] ]
set_PinDelta3 = tableFromList [ [elm "a", elm "b", elm "c"] 
                              , [elm "a", elm "a", elm "c"] ]
set_QinDelta3 = tableFromList [ [elm "a", elm "b"] ]

set_QdiffTinDB3 = tableFromList [ [elm "b", elm "c"] ]

set_PjoinQinDB3 = tableFromList [ [ elm "c", elm "b", elm "c", elm "b"]
                                , [ elm "c", elm "b", elm "c", elm "c"] ]
set_DeltaPjoinQinDB3AndDelta3 = 
    tableFromList [ [ elm "a", elm "b", elm "c", elm "b"]
                  , [ elm "a", elm "b", elm "c", elm "c"] 
                  , [ elm "a", elm "a", elm "c", elm "b"] ]

db_db3    = databaseFromList [(tblRef_P, set_PinDB3), (tblRef_Q, set_QinDB3)
                             ,(tblRef_S, set_SinDB3), (tblRef_T, set_TinDB3)]
db_delta3 = databaseFromList [ (tblRef_P, set_PinDelta3)
                             , (tblRef_Q, set_QinDelta3)]


set_PinDB4 = tableFromList [[elm "a"], [elm "b"]]
setPair_PinDB4 = tablePairFromList $ duplicateRecords [[elm "a"], [elm "b"]]
db_db4     = databaseFromList [(tblRef_P, set_PinDB4)]


set_QinDB5 = tableFromList [ [elm "a", elm "b"]
                           , [elm "b", elm "c"]]
setPair_QinDB5 = tablePairFromList $ 
                 duplicateRecords [ [elm "a", elm "b"]
                                  , [elm "b", elm "c"] ]
db_db5     = databaseFromList [(tblRef_Q, set_QinDB5)]

set_PinDB6 = tableFromList [[elm "a"], [elm "b"]]
set_QinDB6 = tableFromList [ [elm "a", elm "b"]
                           , [elm "b", elm "c"]]
db_db6     = databaseFromList [(tblRef_P, set_PinDB6), (tblRef_Q, set_QinDB6)]

set_PbeforeDB7 = tableFromList [ [elm "a", elm "b"] ]
setPair_PinsertDB7 = tablePairFromList [ ([], [elm "b", elm "c"]) ]
set_PafterDB7  = tableFromList [ [elm "a", elm "b"]
                               , [elm "b", elm "c"] ]
db_beforeDB7 = databaseFromList [(tblRef_P, set_PbeforeDB7)]
db_afterDB7  = databaseFromList [(tblRef_P, set_PafterDB7)]

set_PbeforeDB7_1 = tableFromList [ [elm "a", elm "b"] 
                                 , [elm "a", elm "c"] ]
set_PinsertDB7_1 = tablePairFromList 
                   $ duplicateRecords [ [elm "b", elm "c"]
                                      , [elm "a", elm "b"] ]
setPair_PinsertDB7_1 = tablePairFromList 
                       $ duplicateRecords [ [elm "b", elm "c"]
                                          , [elm "a", elm "b"] ]
set_PafterDB7_1  = tableFromList [ [elm "a", elm "b"]
                                 , [elm "a", elm "c"]
                                 , [elm "b", elm "c"] ]
set_PnewDB7_1  = tableFromList [ [elm "b", elm "c"] ]
db_beforeDB7_1 = databaseFromList [(tblRef_P, set_PbeforeDB7_1)]
db_afterDB7_1  = databaseFromList [(tblRef_P, set_PafterDB7_1)]
db_newDB7_1  = databaseFromList [(tblRef_P, set_PnewDB7_1)]

set_PbeforeDB7_2 = tableFromList [ [elm "a", elm "b"] 
                                 , [elm "a", elm "c"] 
                                 , [elm "a", elm "a"] ]
set_PinsertDB7_2 = tableFromList [ [elm "b"] ]
setPair_PinsertDB7_2 = tablePairFromList $ duplicateRecords [ [elm "b"] ]
set_PafterDB7_2  = tableFromList [ [elm "a", elm "b"]
                                 , [elm "a", elm "c"]
                                 , [elm "a", elm "a"] 
                                 , [elm "b", elm "b"] ]
db_beforeDB7_2 = databaseFromList [(tblRef_P, set_PbeforeDB7_2)]
db_afterDB7_2  = databaseFromList [(tblRef_P, set_PafterDB7_2)]


set_PbeforeDB8 = tableFromList [ [elm "a", elm "b"] ]
set_PinsertDB8 = tableFromList [ [elm "c"] ]
setPair_PinsertDB8 = tablePairFromList $ duplicateRecords [ [elm "c"] ]
set_PafterDB8  = tableFromList [ [elm "a", elm "b"]
                               , [elm "e#0", elm "c"] ]
db_beforeDB8 = databaseFromList [(tblRef_P, set_PbeforeDB8)]
db_afterDB8  = databaseFromList [(tblRef_P, set_PafterDB8)]

set_PbeforeDB9 = tableFromList [ [elm "a", elm "b"] ]
set_PinsertDB9 = tableFromList [ [elm "b"] ]
setPair_PinsertDB9 = tablePairFromList $ duplicateRecords [ [elm "b"] ]
set_PafterDB9  = tableFromList [ [elm "a", elm "b"]
                               , [elm "b", elm "e#0"] ]
db_beforeDB9 = databaseFromList [(tblRef_P, set_PbeforeDB9)]
db_afterDB9  = databaseFromList [(tblRef_P, set_PafterDB9)]

set_PbeforeDB10 = tableFromList []
set_PinsertDB10 = fullTable
setPair_PinsertDB10 = tablePairFromList $ duplicateRecords [[]]
set_PafterDB10  = tableFromList [ [elm "e#0"] ]
db_beforeDB10 = databaseFromList [(tblRef_P, set_PbeforeDB10)]
db_afterDB10  = databaseFromList [(tblRef_P, set_PafterDB10)]

set_PbeforeDB11 = tableFromList [ [elm "a", elm "b"] ]
set_PinsertDB11 = tableFromList [ [elm "b"] ]
setPair_PinsertDB11 = tablePairFromList $ duplicateRecords [ [elm "b"] ]
set_PafterDB11  = tableFromList [ [elm "a", elm "b"] ]
db_beforeDB11 = databaseFromList [(tblRef_P, set_PbeforeDB11)]
db_afterDB11  = databaseFromList [(tblRef_P, set_PafterDB11)]
db_newDB11    = databaseFromList []


set_PbeforeDB12   = tableFromList [ [elm "a", elm "b", elm "c"] 
                                  , [elm "a", elm "a", elm "c"] 
                                  , [elm "c", elm "b", elm "c"] ]
set_QbeforeDB12   = tableFromList [ [elm "a", elm "b"] 
                                  , [elm "b", elm "b"] 
                                  , [elm "b", elm "c"] ]
set_joinInsertDB12 = tableFromList [ [elm "e", elm "f", elm "g", elm "d"] ]
setPair_joinInsertDB12 = 
    tablePairFromList $ duplicateRecords [ [elm "e", elm "f", elm "g", elm "d"] ]
set_PafterDB12   = tableFromList [ [elm "a", elm "b", elm "c"] 
                                 , [elm "a", elm "a", elm "c"] 
                                 , [elm "c", elm "b", elm "c"] 
                                 , [elm "e", elm "f", elm "g"] ]
set_QafterDB12   = tableFromList [ [elm "a", elm "b"] 
                                 , [elm "b", elm "b"] 
                                 , [elm "b", elm "c"] 
                                 , [elm "f", elm "d"] ]
db_beforeDB12 = databaseFromList [ (tblRef_P, set_PbeforeDB12)
                                 , (tblRef_Q, set_QbeforeDB12)]
db_afterDB12  = databaseFromList [ (tblRef_P, set_PafterDB12)
                                 , (tblRef_Q, set_QafterDB12)]

set_PbeforeDB13   = tableFromList [ [elm "a", elm "b", elm "c"] 
                                  , [elm "a", elm "a", elm "c"] 
                                  , [elm "c", elm "b", elm "c"] ]
set_QbeforeDB13   = tableFromList [ [elm "a", elm "b"] 
                                  , [elm "b", elm "b"] 
                                  , [elm "b", elm "c"] ]
set_joinInsertDB13 = tableFromList [ [elm "d", elm "e", elm "f"] ]
setPair_joinInsertDB13 = 
    tablePairFromList $ duplicateRecords [ [elm "d", elm "e", elm "f"] ]
set_PafterDB13   = tableFromList [ [elm "a", elm "b", elm "c"] 
                                 , [elm "a", elm "a", elm "c"] 
                                 , [elm "c", elm "b", elm "c"] 
                                 , [elm "d", elm "e", elm "f"] ]
set_QafterDB13   = tableFromList [ [elm "a", elm "b"] 
                                 , [elm "b", elm "b"] 
                                 , [elm "b", elm "c"] 
                                 , [elm "e", elm "f"] ]
db_beforeDB13 = databaseFromList [ (tblRef_P, set_PbeforeDB13)
                                 , (tblRef_Q, set_QbeforeDB13)]
db_afterDB13  = databaseFromList [ (tblRef_P, set_PafterDB13)
                                 , (tblRef_Q, set_QafterDB13)]

set_QbeforeDB14   = tableFromList [ [elm "a", elm "b"] 
                                  , [elm "a", elm "a"] 
                                  , [elm "c", elm "b"] ]
set_QinsertDB14   = tableFromList [ [elm "a"]
                                  , [elm "c"] ]
setPair_QinsertDB14   = tablePairFromList $ duplicateRecords [ [elm "a"]
                                                             , [elm "c"] ]
set_bafterDB14    = tableFromList [ [elm "e#0" ] ]
set_QafterDB14    = tableFromList [ [elm "a", elm "b"] 
                                  , [elm "a", elm "a"] 
                                  , [elm "c", elm "b"] 
                                  , [elm "a", elm "e#0"]
                                  , [elm "c", elm "e#0"] ]

db_beforeDB14 = databaseFromList [ (tblRef_Q, set_QbeforeDB14)]
db_afterDB14  = databaseFromList [ (tblRef_b, set_bafterDB14)
                                 , (tblRef_Q, set_QafterDB14)]

set_PbeforeDB15   = tableFromList [ [elm "a", elm "b", elm "c"] 
                                  , [elm "a", elm "a", elm "c"] 
                                  , [elm "c", elm "b", elm "c"] ]

set_PinsertDB15   = tableFromList [ [elm "a"]
                                  , [elm "b"]
                                  , [elm "a"] ]
setPair_PinsertDB15 = tablePairFromList $ duplicateRecords [ [elm "a"]
                                                           , [elm "b"]
                                                           , [elm "a"] ]

set_cafterDB15   = tableFromList [ [elm "e#0"] ] 
set_PafterDB15   = tableFromList [ [elm "a", elm "b", elm "c"] 
                                 , [elm "a", elm "a", elm "c"] 
                                 , [elm "c", elm "b", elm "c"] 
                                 , [elm "a", elm "e#0", elm "a"] 
                                 , [elm "b", elm "e#0", elm "b"] ]
db_beforeDB15 = databaseFromList [ (tblRef_P, set_PbeforeDB15) ]
db_afterDB15  = databaseFromList [ (tblRef_c, set_cafterDB15)
                                 , (tblRef_P, set_PafterDB15)  ]



set_ainDB16   = tableFromList [ [elm "a"] ]
set_binDB16   = tableFromList [ [elm "b"] ]
set_cinDB16   = tableFromList [ [elm "c"] ]

set_aunionbinDB16   = tableFromList [ [elm "a"] 
                                    , [elm "b"] ]

set_PinDB16   = tableFromList [ [elm "a", elm "b"] 
                              , [elm "b", elm "a"]
                              , [elm "a", elm "c"] 
                              , [elm "c", elm "b"] ]

set_QinDB16   = tableFromList [ [elm "a", elm "b"] 
                              , [elm "c", elm "b"] ]

set_SinDB16   = tableFromList [ [elm "a"], [elm "b"] ]

set_PxydiffQxy   = tableFromList [ [elm "b", elm "a"]
                                 , [elm "a", elm "c"] ]
setPair_PxydiffQxy = tablePairFromList 
                     $ duplicateRecords [ [elm "b", elm "a"]
                                        , [elm "a", elm "c"] ]

set_PxydiffQyx    = tableFromList [ [elm "b", elm "a"] 
                                  , [elm "c", elm "a"] 
                                  , [elm "b", elm "c"] ]
setPair_PxydiffQyx  = 
    tablePairFromList [ ([elm "a", elm "b"], [elm "b", elm "a"]) 
                      , ([elm "a", elm "c"], [elm "c", elm "a"])
                      , ([elm "c", elm "b"], [elm "b", elm "c"]) ]

set_PxyQyzdiffQyx     = tableFromList [ [elm "c", elm "a"] ]
setPair_PxyQyzdiffQyx = 
    tablePairFromList [ ([elm "a", elm "c", elm "b"], [elm "c", elm "a"]) ]

set_PxydiffSx    = tableFromList [ [elm "c"] ]

db_db16 = databaseFromList [ (tblRef_a, set_ainDB16), (tblRef_b, set_binDB16)
                           , (tblRef_c, set_cinDB16)
                           , (tblRef_P, set_PinDB16), (tblRef_Q, set_QinDB16)
                           , (tblRef_S, set_SinDB16) ]


set_PinDB17_1   = tableFromList [ [elm "a", elm "b"] ]
db_db17_1       = databaseFromList [(tblRef_P, set_PinDB17_1)]

set_QinDB17_2   = tableFromList [ [elm "a", elm "b", elm "c"] ]
db_db17_2       = databaseFromList [(tblRef_Q, set_QinDB17_2)]

db_db17_1union2 = databaseFromList [ (tblRef_P, set_PinDB17_1)
                                   , (tblRef_Q, set_QinDB17_2)]

set_PinDB18_1   = tableFromList [ [elm "a", elm "b"] ]
set_QinDB18_1   = tableFromList [ [elm "a", elm "a", elm "a"] 
                                , [elm "a", elm "b", elm "c"] ]
db_db18_1       = databaseFromList [ (tblRef_P, set_PinDB18_1)
                                   , (tblRef_Q, set_QinDB18_1)]

set_PinDB18_2   = tableFromList [ [elm "a", elm "b"] ]
set_QinDB18_2   = tableFromList [ [elm "a", elm "b", elm "c"] ]
db_db18_2       = databaseFromList [ (tblRef_Q, set_QinDB18_2)
                                   , (tblRef_P, set_PinDB18_2)]

set_QinDB18_1union2   = tableFromList [ [elm "a", elm "b", elm "c"] 
                                      , [elm "a", elm "a", elm "a"]]
db_db18_1union2 = databaseFromList [ (tblRef_P, set_PinDB18_1)
                                   , (tblRef_Q, set_QinDB18_1union2)]

set_QinDB18_1diff2 = tableFromList [ [elm "a", elm "a", elm "a"] ]
db_db18_1diff2     = databaseFromList [(tblRef_Q, set_QinDB18_1diff2)]


set_PinDB19   = tableFromList []
set_QinDB19   = tableFromList [ [elm "a", elm "b"]
                              , [elm "b", elm "c"] ]
set_RinDB19   = tableFromList []
db_db19_1     = databaseFromList [ (tblRef_P, set_PinDB19)
                                 , (tblRef_Q, set_QinDB19)
                                 , (tblRef_R, set_RinDB19)]
db_db19_2     = databaseFromList [ (tblRef_Q, set_QinDB19) ]
db_db19_3     = databaseFromList [ (tblRef_P, set_PinDB19)
                                 , (tblRef_R, set_RinDB19)]

set_PinDelta20 = tableFromList [ [elm "a", elm "b", elm "c"] 
                               , [elm "a", elm "a", elm "c"] ]
set_QinDelta20 = tableFromList [ [elm "a", elm "b"] ]


set_DeltaPjoinQinDB20AndDelta20 = 
    tableFromList [ [ elm "a", elm "a", elm "c", elm "b"] ]

db_db20    = databaseFromList []
db_delta20 = databaseFromList [ (tblRef_P, set_PinDelta20)
                              , (tblRef_Q, set_QinDelta20)]
