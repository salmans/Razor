{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

{- Razor
   Module      : TPTP.Test.TestData
   Description : Test data for testing TPTP translation
   Maintainer  : Salman Saghafi -}

module TPTP.Test.TestData where

-- Standard
import qualified Data.Map as Map
import qualified Data.Bimap as Bimap

-- Syntax
import Syntax.Term (Term (..), Element (Element), Constant (Constant))

-- Common
import Common.Provenance (ProvInfo (ProvInfo))

-- Chase
import Chase.Data
import Chase.PossibleFacts.RelAlg.Lang 
    (emptyTable, tableFromList, databaseFromList)
import Chase.PossibleFacts.RelAlg.PossibleFacts (RelSequent)

-- SAT
import SAT.Data

-- Tools
import qualified Tools.ExtendedSet as ExSet

-- Test
import Chase.PossibleFacts.RelAlg.Test.TestData
import Common.Test.TestData

-- Helpers
elm  = Element
cons = Constant
consTerm = Cons . Constant

thy_triv      = ["Truth => Falsehood"]

propSeqs_triv = [ PropSequent [] [] ]
propThy_triv  = PropTheory (ExSet.fromList propSeqs_triv) Bimap.empty 1
-------------------------

thy_thy101 = ["P(x) => Q(x)", "Q(x) => R(x)"]

propThy_thy101 = PropTheory ExSet.empty Bimap.empty 1
-------------------------

thy_thy102 = ["exists someP x . P(x)"]
propObs_thy102  = Bimap.fromList [ (obs_Pe0, 1), (obs_ElemE0, 2)
                                 , (obs_EqlE0E0, 3)]
propSeqs_thy102 = [ PropSequent [] [[2,1]] 
                  , PropSequent [1,3] [[1]]
                  , PropSequent [2] [[3]]
                  , PropSequent [3] [[3]]
                  , PropSequent [3,3] [[3]]
                  ]
propThy_thy102  = PropTheory (ExSet.fromList propSeqs_thy102) 
                  propObs_thy102 4

set_PinRel102 = tableFromList [ [elm "e#0"] ]
set_EqinRel102 = tableFromList [ [elm "e#0", elm "e#0"] ]
set_ElementinRel102 = tableFromList [ [elm "e#0"] ]
base_rel102   = databaseFromList [(tblRef_P, set_PinRel102)
                                 ,(tblRef_Element, set_ElementinRel102)
                                 ,(tblRef_Eq, set_EqinRel102)]
prov_rel102   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "someP") ])
-------------------------

thy_thy103 = ["P('a)"]
propObs_thy103  = Bimap.fromList [ (obs_Pe0, 1), (obs_ElemE0, 2)
                                 , (obs_EqlE0E0, 3)]
propSeqs_thy103 = [ PropSequent [] [[2,1]] 
                  , PropSequent [1,3] [[1]]
                  , PropSequent [2] [[3]]
                  , PropSequent [3] [[3]]
                  , PropSequent [3,3] [[3]]
                  ]
propThy_thy103  = PropTheory (ExSet.fromList propSeqs_thy103)
                  propObs_thy103 4

set_ainRel103 = tableFromList [ [elm "e#0"] ]
set_PinRel103 = tableFromList [ [elm "e#0"] ]
set_EqinRel103 = tableFromList [ [elm "e#0", elm "e#0"] ]
set_ElementinRel103 = tableFromList [ [elm "e#0"] ]
base_rel103   = databaseFromList [ (tblRef_a, set_ainRel103)
                                 , (tblRef_P, set_PinRel103) 
                                 , (tblRef_Eq, set_EqinRel103)
                                 , (tblRef_Element, set_ElementinRel103)]
prov_rel103   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "a") ])
-------------------------

thy_thy104 = [ "P(a())"
             , "P(a()) => Q(a())" ]
propObs_thy104  = Bimap.fromList [ (obs_Pe0, 1), (obs_ElemE0, 2)
                                 , (obs_Qe0, 3), (obs_EqlE0E0, 4) ]
propSeqs_thy104 = [ PropSequent [] [[2,1]] 
                  , PropSequent [1] [[2,3]]
                  , PropSequent [1,4] [[1]]
                  , PropSequent [3,4] [[3]]
                  , PropSequent [2] [[4]]
                  , PropSequent [4] [[4]]
                  , PropSequent [4,4] [[4]] 
                  ]
propThy_thy104  = PropTheory (ExSet.fromList propSeqs_thy104)
                  propObs_thy104 5
set_ainRel104 = tableFromList [ [elm "e#0"] ]
set_PinRel104 = tableFromList [ [elm "e#0"] ]
set_QinRel104 = tableFromList [ [elm "e#0"] ]
set_EqinRel104 = tableFromList [ [elm "e#0", elm "e#0"] ]
set_ElementinRel104 = tableFromList [ [elm "e#0"] ]
base_rel104   = databaseFromList [ (tblRef_a, set_ainRel104)
                                 , (tblRef_P, set_PinRel104)
                                 , (tblRef_Q, set_QinRel104)
                                 , (tblRef_Eq, set_EqinRel104)
                                 , (tblRef_Element, set_ElementinRel104)]
prov_rel104   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "a") ])
-------------------------

thy_thy0 = [ "Truth => exists someR0 y . exists someR1 z.  R(y,z)"]
propObs_thy0  = Bimap.fromList [ (obs_Re0e1, 1), (obs_ElemE0, 2)
                               , (obs_ElemE1, 3)
                               , (obs_EqlE1E1, 4), (obs_EqlE0E0, 5) ]
propSeqs_thy0 = [ PropSequent [] [[3,2,1]] 
                , PropSequent [1,4] [[1]]
                , PropSequent [1,5] [[1]]
                , PropSequent [2] [[5]]
                , PropSequent [3] [[4]]
                , PropSequent [4] [[4]]
                , PropSequent [5] [[5]]
                , PropSequent [4,4] [[4]] 
                , PropSequent [5,5] [[5]] 
                ]
propThy_thy0  = PropTheory (ExSet.fromList propSeqs_thy0)
                  propObs_thy0 6

set_RinRel0 = tableFromList [ [elm "e#0", elm "e#1"] ]
set_EqinRel0 = tableFromList [ [elm "e#0", elm "e#0"] 
                             , [elm "e#1", elm "e#1"]]
set_ElementinRel0 = tableFromList [ [elm "e#0"], [elm "e#1"] ]
base_rel0   = databaseFromList [ (tblRef_R, set_RinRel0) 
                               , (tblRef_Eq, set_EqinRel0) 
                               , (tblRef_Element, set_ElementinRel0)]
prov_rel0   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "someR0") 
                                       , (elm "e#1", consTerm "someR1") ])
-------------------------

thy_thy1 = [ "Truth => exists someR y. R(y)"
           , "R(x) =>  exists someQ z . Q(z) & R(x)" ]
propObs_thy1  = Bimap.fromList [ (obs_Re0, 1), (obs_ElemE0, 2)
                               , (obs_ElemE1, 3), (obs_Qe1, 4)
                               , (obs_EqlE0E0, 5), (obs_EqlE1E1, 6) ]
propSeqs_thy1 = [ PropSequent [] [[2,1]] 
                , PropSequent [1] [[4,3,1]]
                , PropSequent [1,5] [[1]]
                , PropSequent [4,6] [[4]]
                , PropSequent [2] [[5]]
                , PropSequent [3] [[6]]
                , PropSequent [5] [[5]]
                , PropSequent [6] [[6]]
                , PropSequent [5,5] [[5]] 
                , PropSequent [6,6] [[6]] 
                ]
propThy_thy1  = PropTheory (ExSet.fromList propSeqs_thy1)
                  propObs_thy1 7

set_RinRel1 = tableFromList [ [elm "e#0"] ]
set_QinRel1 = tableFromList [ [elm "e#1"] ]
set_EqinRel1 = tableFromList [ [elm "e#0", elm "e#0"] 
                             , [elm "e#1", elm "e#1"]]
set_ElementinRel1 = tableFromList [ [elm "e#0"], [elm "e#1"] ]
base_rel1   = databaseFromList [ (tblRef_Q, set_QinRel1) 
                               , (tblRef_R, set_RinRel1) 
                               , (tblRef_Eq, set_EqinRel1)
                               , (tblRef_Element, set_ElementinRel1)]
prov_rel1   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "someR") 
                                       , (elm "e#1"
                                         , Fn "someQ" [consTerm "someR"]) ])
-------------------------

thy_thy2 = [ "Truth => exists someRU y . R(y) & U(y)"
           , "U(u) & R(x) => exists someQ z . Q(x,u)"]
propObs_thy2  = Bimap.fromList [ (obs_Ue0, 1), (obs_ElemE0, 2)
                               , (obs_Re0, 3), (obs_Qe0e0, 4)
                               , (obs_ElemE1, 5)
                               , (obs_EqlE0E0, 6), (obs_EqlE1E1, 7) ]
propSeqs_thy2 = [ PropSequent [] [[3,2,1]] 
                , PropSequent [1,3] [[5,4]]
                , PropSequent [1,6] [[1]]
                , PropSequent [3,6] [[3]]
                , PropSequent [4,6] [[4]]
                , PropSequent [2] [[6]]
                , PropSequent [5] [[7]]
                , PropSequent [6] [[6]]
                , PropSequent [7] [[7]]
                , PropSequent [6,6] [[6]] 
                , PropSequent [7,7] [[7]] 
                ]
propThy_thy2  = PropTheory (ExSet.fromList propSeqs_thy2)
                  propObs_thy2 8

set_QinRel2 = tableFromList [ [elm "e#0", elm "e#0"] ]
set_RinRel2 = tableFromList [ [elm "e#0"] ]
set_UinRel2 = tableFromList [ [elm "e#0"] ]
set_EqinRel2 = tableFromList [ [elm "e#0", elm "e#0"] 
                             , [elm "e#1", elm "e#1"] ]
set_ElementinRel2 = tableFromList [ [elm "e#0"], [elm "e#1"] ]
base_rel2   = databaseFromList [ (tblRef_Q, set_QinRel2) 
                               , (tblRef_R, set_RinRel2) 
                               , (tblRef_U, set_UinRel2) 
                               , (tblRef_Eq, set_EqinRel2)
                               , (tblRef_Element, set_ElementinRel2)]

prov_rel2   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "someRU") 
                                       , (elm "e#1"
                                         , Fn "someQ" [ consTerm "someRU"
                                                      , consTerm "someRU"]) ])
-------------------------

thy_thy3 = [ "Truth => exists someR y . R(y)"
           , "R(x) => exists someQ w . Q(x,w)" ]
propObs_thy3  = Bimap.fromList [ (obs_Re0, 1), (obs_ElemE0, 2)
                               , (obs_Qe0e1, 3), (obs_ElemE1, 4)
                               , (obs_EqlE0E0, 5), (obs_EqlE1E1, 6) ]
propSeqs_thy3 = [ PropSequent [] [[2,1]] 
                , PropSequent [1] [[4,3]]
                , PropSequent [1,5] [[1]]
                , PropSequent [3,5] [[3]]
                , PropSequent [3,6] [[3]]
                , PropSequent [2] [[5]]
                , PropSequent [4] [[6]]
                , PropSequent [5] [[5]]
                , PropSequent [6] [[6]]
                , PropSequent [5,5] [[5]] 
                , PropSequent [6,6] [[6]] 
                ]
propThy_thy3  = PropTheory (ExSet.fromList propSeqs_thy3)
                  propObs_thy3 7

set_QinRel3 = tableFromList [ [elm "e#0", elm "e#1"] ]
set_RinRel3 = tableFromList [ [elm "e#0"] ]
set_EqinRel3 = tableFromList [ [elm "e#0", elm "e#0"] 
                               , [elm "e#1", elm "e#1"]]
set_ElementinRel3 = tableFromList [ [elm "e#0"], [elm "e#1"] ]
base_rel3   = databaseFromList [ (tblRef_Q, set_QinRel3) 
                               , (tblRef_R, set_RinRel3) 
                               , (tblRef_Eq, set_EqinRel3)
                               , (tblRef_Element, set_ElementinRel3)]
prov_rel3   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "someR") 
                                       , (elm "e#1"
                                         , Fn "someQ" [consTerm "someR"]) ])
-------------------------

thy_thy4 = [ "Truth => exists someQ y. Q(y)"
           , "Q(x) => R(x) | S(x)" ]
propObs_thy4  = Bimap.fromList [ (obs_Qe0, 1), (obs_ElemE0, 2)
                               , (obs_Se0, 3), (obs_Re0, 4)
                               , (obs_EqlE0E0, 5) ]
propSeqs_thy4 = [ PropSequent [] [[2,1]] 
                , PropSequent [1] [[4],[3]]
                , PropSequent [1,5] [[1]]
                , PropSequent [3,5] [[3]]
                , PropSequent [4,5] [[4]]
                , PropSequent [2] [[5]]
                , PropSequent [5] [[5]]
                , PropSequent [5,5] [[5]]
                ]
propThy_thy4  = PropTheory (ExSet.fromList propSeqs_thy4)
                  propObs_thy4 6

set_QinRel4 = tableFromList [ [elm "e#0"] ]
set_RinRel4 = tableFromList [ [elm "e#0"] ]
set_SinRel4 = tableFromList [ [elm "e#0"] ]
set_EqinRel4 = tableFromList [ [elm "e#0", elm "e#0"] ]
set_ElementinRel4 = tableFromList [ [elm "e#0"] ]
base_rel4   = databaseFromList [ (tblRef_Q, set_QinRel4) 
                               , (tblRef_R, set_RinRel4)
                               , (tblRef_S, set_SinRel4) 
                               , (tblRef_Eq, set_EqinRel4)
                               , (tblRef_Element, set_ElementinRel4)]
prov_rel4   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "someQ") ])
-------------------------

thy_thy5 = [ "Truth => exists someQ y. Q(y)"
           , "Q(x) => R(x) | S(x)"
           , "R(x) => Falsehood" ]
propObs_thy5  = Bimap.fromList [ (obs_Qe0, 1), (obs_ElemE0, 2)
                               , (obs_Se0, 3), (obs_Re0, 4)
                               , (obs_EqlE0E0, 5) ]
propSeqs_thy5 = [ PropSequent [] [[2,1]] 
                , PropSequent [1] [[4],[3]]
                , PropSequent [4] []
                , PropSequent [1,5] [[1]]
                , PropSequent [3,5] [[3]]
                , PropSequent [4,5] [[4]]
                , PropSequent [2] [[5]]
                , PropSequent [5] [[5]]
                , PropSequent [5,5] [[5]]
                ]
propThy_thy5  = PropTheory (ExSet.fromList propSeqs_thy5)
                  propObs_thy5 6

set_QinRel5 = tableFromList [ [elm "e#0"] ]
set_RinRel5 = tableFromList [ [elm "e#0"] ]
set_SinRel5 = tableFromList [ [elm "e#0"] ]
set_EqinRel5 = tableFromList [ [elm "e#0", elm "e#0"] ]
set_ElementinRel5 = tableFromList [ [elm "e#0"] ]
base_rel5   = databaseFromList [ (tblRef_Q, set_QinRel5) 
                               , (tblRef_R, set_RinRel5)
                               , (tblRef_S, set_SinRel5) 
                               , (tblRef_Eq, set_EqinRel5)
                               , (tblRef_Element, set_ElementinRel5) ]
prov_rel5   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "someQ") ])
-------------------------

thy_thy6 = [ "Truth => exists someQ a . Q(a)"
           , "Q(x) => R(x,x)"
           , "R(x,y) => exists e d. Q(e) & R(d,e)" ]
propObs_thy6  = Bimap.fromList [ (obs_Qe0, 1), (obs_ElemE0, 2)
                               , (obs_Re0e0, 3), (obs_EqlE0E0, 4)]
propSeqs_thy6 = [ PropSequent [] [[2,1]] 
                , PropSequent [1] [[3]]
                , PropSequent [1,4] [[1]]
                , PropSequent [3,4] [[3]]
                , PropSequent [2] [[4]]
                , PropSequent [4] [[4]]
                , PropSequent [4,4] [[4]]
                ]
propThy_thy6  = PropTheory (ExSet.fromList propSeqs_thy6)
                  propObs_thy6 5

set_QinRel6 = tableFromList [ [elm "e#0"] ]
set_RinRel6 = tableFromList [ [elm "e#0", elm "e#0"] ]
set_EqinRel6 = tableFromList [ [elm "e#0", elm "e#0"] ]
set_ElementinRel6 = tableFromList [ [elm "e#0"] ]
base_rel6   = databaseFromList [ (tblRef_Q, set_QinRel6) 
                               , (tblRef_R, set_RinRel6) 
                               , (tblRef_Eq, set_EqinRel6)
                               , (tblRef_Element, set_ElementinRel6) ]
prov_rel6   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "someQ") ])
-------------------------

thy_thy7 = [ "Truth => exists someQ a . Q(a)"
           , "Q(x) => R(x,x) | S(x)"
           , "Q(x) => R(x,x)"
           , "R(x,y) => exists b  . S(b)"
           , "R(x,x) => exists e . Q(e) & R(e,x)" ]
propObs_thy7  = Bimap.fromList [ (obs_Qe0, 1), (obs_ElemE0, 2)
                               , (obs_Se0, 3), (obs_Re0e0, 4)
                               , (obs_EqlE0E0, 5)]
propSeqs_thy7 = [ PropSequent [] [[2,1]] 
                , PropSequent [1] [[4],[3]]
                , PropSequent [1] [[4]]
                , PropSequent [4] [[3]] --
                , PropSequent [4] [[4], [2]] --
                , PropSequent [1,5] [[1]]
                , PropSequent [3,5] [[3]]
                , PropSequent [4,5] [[4]]
                , PropSequent [2] [[5]]
                , PropSequent [5] [[5]]
                , PropSequent [5,5] [[5]]
                ]
propThy_thy7  = PropTheory (ExSet.fromList propSeqs_thy7)
                  propObs_thy7 6

set_QinRel7 = tableFromList [ [elm "e#0"] ]
set_RinRel7 = tableFromList [ [elm "e#0", elm "e#0"] ]
set_SinRel7 = tableFromList [ [elm "e#0"] ]
set_EqinRel7 = tableFromList [ [elm "e#0", elm "e#0"] ]
set_ElementinRel7 = tableFromList [ [elm "e#0"] ]
base_rel7   = databaseFromList [ (tblRef_Q, set_QinRel7) 
                               , (tblRef_R, set_RinRel7) 
                               , (tblRef_S, set_SinRel7) 
                               , (tblRef_Eq, set_EqinRel7)
                               , (tblRef_Element, set_ElementinRel7)]
prov_rel7   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "someQ") ])

thy_thy8 = [ "Truth => P"
           , "P => Q |R"
           , "Q => P | S"
           , "S => Falsehood" ]
set_PropinRel8 = tableFromList [ [] ]
base_rel8   = databaseFromList [ (tblRef_P, set_PropinRel8) 
                               , (tblRef_Q, set_PropinRel8) 
                               , (tblRef_R, set_PropinRel8) 
                               , (tblRef_S, set_PropinRel8) ]


thy_thy9 = [ "Truth => P(a(),b())"
           , "Truth => T(c())"
           , "(P(x,y) & Q(y,z)) => R(x,z)"
           , "(R(x,y) & S(x)) => Falsehood"
           , "(P(x,y) & T(z)) => Q(y,z)" ]
set_ainRel9 = tableFromList [ [elm "e#0"] ]
set_binRel9 = tableFromList [ [elm "e#1"] ]
set_cinRel9 = tableFromList [ [elm "e#2"] ]
set_PinRel9 = tableFromList [ [elm "e#0", elm "e#1"] ]
set_QinRel9 = tableFromList [ [elm "e#1", elm "e#2"] ]
set_RinRel9 = tableFromList [ [elm "e#0", elm "e#2"] ]
set_TinRel9 = tableFromList [ [elm "e#2"] ]
set_EqinRel9 = tableFromList [ [elm "e#0", elm "e#0"] 
                             , [elm "e#1", elm "e#1"] 
                             , [elm "e#2", elm "e#2"] ]
set_ElementinRel9 = tableFromList [ [elm "e#0"], [elm "e#1"], [elm "e#2"] ]
base_rel9   = databaseFromList [ (tblRef_a, set_ainRel9) 
                               , (tblRef_b, set_binRel9) 
                               , (tblRef_c, set_cinRel9) 
                               , (tblRef_P, set_PinRel9) 
                               , (tblRef_Q, set_QinRel9) 
                               , (tblRef_R, set_RinRel9) 
                               , (tblRef_T, set_TinRel9) 
                               , (tblRef_Eq, set_EqinRel9)
                               , (tblRef_Element, set_ElementinRel9)]
prov_rel9   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "a") 
                                       , (elm "e#1", consTerm "b")
                                       , (elm "e#2", consTerm "c") ])

thy_thy14 = [ "P(a())"
            , "x = a() => Q(x)"
            , "a() = x => R(x)"
            , "a() = a() => S(a())" ]
set_ainRel14 = tableFromList [ [elm "e#0"] ]
set_PinRel14 = tableFromList [ [elm "e#0"] ]
set_QinRel14 = tableFromList [ [elm "e#0"] ]
set_RinRel14 = tableFromList [ [elm "e#0"] ]
set_SinRel14 = tableFromList [ [elm "e#0"] ]
set_EqinRel14 = tableFromList [ [elm "e#0", elm "e#0"] ]
set_ElementinRel14 = tableFromList [ [elm "e#0"] ]
base_rel14  = databaseFromList [ (tblRef_a, set_ainRel14) 
                               , (tblRef_P, set_PinRel14) 
                               , (tblRef_Q, set_QinRel14) 
                               , (tblRef_R, set_RinRel14) 
                               , (tblRef_S, set_SinRel14) 
                               , (tblRef_Eq, set_EqinRel14)
                               , (tblRef_Element, set_ElementinRel14)]
prov_rel14   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "a") ])

-- thy_thy15 = [ "R6(fv6) => R3(fv6)"
--             , "R0(c1()) => exists bv0. exists bv1. R8(c4(),bv1,bv1)"
--             , "R4(fv7,fv6) => (Falsehood & exists bv0. R3(fv6))"
--             , "Truth => R6(c4())"
--             , "R3(fv5) => (R4(c6(),fv5) & R9(fv5))"
--             , "R8(c3(),c9(),c9()) => R7(c4(),c6())" ]

thy_thy16 = [ "exists someElem x. exists anotherElem y. x = y" ]
set_EqinRel16 = tableFromList [ [elm "e#0", elm "e#0"] 
                              , [elm "e#0", elm "e#1"]
                              , [elm "e#1", elm "e#0"] 
                              , [elm "e#1", elm "e#1"] ]
set_ElementinRel16 = tableFromList [ [elm "e#0"], [elm "e#1"]]
base_rel16  = databaseFromList [ (tblRef_Eq, set_EqinRel16)
                               , (tblRef_Element, set_ElementinRel16)]
prov_rel16   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "someElem") 
                                        , (elm "e#1", consTerm "anotherElem")])

thy_thy17 = [ "P(a()) & P(b()) & f(a()) = b() & f(b()) = a()"
            , "P(x) => Q(b()) | Q(f(x))" ]

list_EqClass0inRel17 = [elm "e#0", elm "e#2"]
list_EqClass1inRel17 = [elm "e#1", elm "e#3"]
set_ainRel17 = tableFromList [ [elm "e#2"] ]
set_binRel17 = tableFromList [ [elm "e#3"] ]
set_fnfinRel17 = tableFromList $ 
                 [ [x, y] | x <- list_EqClass0inRel17
                 , y <- list_EqClass1inRel17 ] ++
                 [ [x, y] | x <- list_EqClass1inRel17
                 , y <- list_EqClass0inRel17 ]
set_PinRel17 = tableFromList $ 
               [ [x] | x <- list_EqClass0inRel17 ] ++
               [ [x] | x <- list_EqClass1inRel17 ]
set_QinRel17 = tableFromList $ 
               [ [x] | x <- list_EqClass0inRel17 ] ++
               [ [x] | x <- list_EqClass1inRel17 ]
set_EqinRel17 = tableFromList $ 
                 [ [x, y] | x <- list_EqClass0inRel17
                 , y <- list_EqClass0inRel17 ] ++
                 [ [x, y] | x <- list_EqClass1inRel17
                 , y <- list_EqClass1inRel17 ]
set_ElementinRel17 = tableFromList [ [elm "e#0"], [elm "e#1"]
                                   , [elm "e#2"], [elm "e#3"] ]
base_rel17   = databaseFromList [ (tblRef_a, set_ainRel17) 
                                , (tblRef_b, set_binRel17)                   
                                , (tblRef_P, set_PinRel17) 
                                , (tblRef_Q, set_QinRel17) 
                                , (fnTblRef_f, set_fnfinRel17) 
                                , (tblRef_Eq, set_EqinRel17) 
                                , (tblRef_Element, set_ElementinRel17) ]
prov_rel17   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "f#3") 
                                        , (elm "e#1", consTerm "f#1")
                                        , (elm "e#2", consTerm "a") 
                                        , (elm "e#3", consTerm "b") ])

thy_thy18 = [ "exists elem1 x. exists elem2 y. exists elem3 z. R(x,y,z)"
            , "a = b" ]
list_EqClass1inRel18 = [elm "e#0", elm "e#1", elm "e#2"]
set_RinRel18 = tableFromList [ [x, y, z] | x <- list_EqClass1inRel18
                             , y <- list_EqClass1inRel18
                             , z <- list_EqClass1inRel18 ]
set_EqinRel18 = tableFromList [ [x, y] | x <- list_EqClass1inRel18
                              , y <- list_EqClass1inRel18 ]

set_ElementinRel18 = tableFromList [ [elm "e#0"], [elm "e#1"], [elm "e#2"] ]
base_rel18   = databaseFromList [ (tblRef_R, set_RinRel18) 
                                , (tblRef_Eq, set_EqinRel18) 
                                , (tblRef_Element, set_ElementinRel18) ]
prov_rel18   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "elem1") 
                                        , (elm "e#1", consTerm "elem2")
                                        , (elm "e#2", consTerm "elem3") ])

thy_thyf0 = [ "Truth => exists someQ a . Q(a)"
            , "Q(x) => R(x,f(x))"]
set_QinRelf0 = tableFromList [ [elm "e#0"] ]
set_RinRelf0 = tableFromList [ [elm "e#0", elm "e#1"] ]
set_fnfinRelf0 = tableFromList [ [elm "e#0", elm "e#1"] ]
set_EqinRelf0 = tableFromList [ [elm "e#0", elm "e#0"] 
                             , [elm "e#1", elm "e#1"] ]
set_ElementinRelf0 = tableFromList [ [elm "e#0"], [elm "e#1"] ]
base_relf0   = databaseFromList [ (tblRef_Q, set_QinRelf0) 
                                , (tblRef_R, set_RinRelf0) 
                                , (fnTblRef_f, set_fnfinRelf0) 
                                , (tblRef_Eq, set_EqinRelf0)
                                , (tblRef_Element, set_ElementinRelf0)]
prov_relf0   = ProvInfo (Bimap.fromList [ (elm "e#0", consTerm "someQ") 
                                        , (elm "e#1"
                                          , Fn "f#1" [consTerm "someQ"]) ])

thy_thyf1 = ["Truth => exists someQ a . Q(f(a))"
            , "Q(x) => R(x, f(f(x)))" ]
set_QinRelf1 = tableFromList [ [elm "e#0"] ]
set_RinRelf1 = tableFromList [ [elm "e#0", elm "e#3"] ]
set_fnfinRelf1 = tableFromList [ [elm "e#1", elm "e#0"] 
                               , [elm "e#0", elm "e#2"]
                               , [elm "e#2", elm "e#3"] ]
set_EqinRelf1 = tableFromList [ [elm "e#0", elm "e#0"] 
                              , [elm "e#1", elm "e#1"] 
                              , [elm "e#2", elm "e#2"]
                              , [elm "e#3", elm "e#3"] ]
set_ElementinRelf1 = tableFromList [ [elm "e#0"], [elm "e#1"] 
                                   , [elm "e#2"], [elm "e#3"] ]
base_relf1   = databaseFromList [ (tblRef_Q, set_QinRelf1) 
                                , (tblRef_R, set_RinRelf1) 
                                , (fnTblRef_f, set_fnfinRelf1) 
                                , (tblRef_Eq, set_EqinRelf1)
                                , (tblRef_Element, set_ElementinRelf1)]
prov_relf1   = ProvInfo (Bimap.fromList 
                         [ (elm "e#0", consTerm "f#1") 
                         , (elm "e#1", consTerm "someQ") 
                         , (elm "e#2", Fn "f#3" [consTerm "f#1"])
                         , (elm "e#3", Fn "f#5" [consTerm "f#1"]) ])

thy_thyf2 = [ "Truth => exists someQ a . Q(a)"
           , "Q(x) => g(x) = f(x)" ]
set_QinRelf2 = tableFromList [ [elm "e#0"] ]
set_fnfinRelf2 = tableFromList [ [elm "e#0", elm "e#1"] 
                               , [elm "e#0", elm "e#2"] ]
set_fnginRelf2 = tableFromList [ [elm "e#0", elm "e#1"] 
                               , [elm "e#0", elm "e#2"] ]

set_EqinRelf2 = tableFromList [ [elm "e#0", elm "e#0"] 
                              , [elm "e#1", elm "e#1"] 
                              , [elm "e#2", elm "e#2"] 
                              , [elm "e#1", elm "e#2"]
                              , [elm "e#2", elm "e#1"] ]
set_ElementinRelf2 = tableFromList [ [elm "e#0"], [elm "e#1"], [elm "e#2"] ]
base_relf2   = databaseFromList [ (tblRef_Q, set_QinRelf2) 
                                , (fnTblRef_f, set_fnfinRelf2) 
                                , (fnTblRef_g, set_fnginRelf2) 
                                , (tblRef_Eq, set_EqinRelf2)
                                , (tblRef_Element, set_ElementinRelf2)]
prov_relf2   = ProvInfo (Bimap.fromList 
                         [ (elm "e#0", consTerm "someQ") 
                         , (elm "e#1", Fn "f#3" [consTerm "someQ"])
                         , (elm "e#2", Fn "g#1" [consTerm "someQ"]) ])

thy_thyf3 = [ "Truth => exists someQf a . Q(f(a))"
            , "Q(f(x)) => R(x,f(x)) "
            , "Q(x) => R(x, f(f(x)))"
            , "R(x, f(f(x))) => exists someS b . S(b) & P(x)" ]

list_EqClass0inRelf3 = [elm "e#0"]

set_PinRelf3 = tableFromList [ [ elm "e#0" ] ]
set_QinRelf3 = tableFromList [ [ elm "e#0" ] ]
set_RinRelf3 = tableFromList $ 
               [ [elm "e#1", x] | x <- list_EqClass0inRelf3 ]
               ++ [ [x, elm "e#3"] | x <- list_EqClass0inRelf3 ]
set_SinRelf3 = tableFromList [ [elm "e#4"] ]
set_fnfinRelf3 = tableFromList [ [elm "e#1", elm "e#0"] 
                               , [elm "e#0", elm "e#2"] 
                               , [elm "e#2", elm "e#3"] ]
set_EqinRelf3 = tableFromList [ [elm "e#0", elm "e#0"] 
                              , [elm "e#1", elm "e#1"] 
                              , [elm "e#2", elm "e#2"] 
                              , [elm "e#3", elm "e#3"] 
                              , [elm "e#4", elm "e#4"] ]
set_ElementinRelf3 = tableFromList [ [elm "e#0"], [elm "e#1"], [elm "e#2"] 
                                   , [elm "e#3"], [elm "e#4"] ]

base_relf3   = databaseFromList [ (fnTblRef_f, set_fnfinRelf3) 
                                , (tblRef_P, set_PinRelf3)
                                , (tblRef_Q, set_QinRelf3)
                                , (tblRef_R, set_RinRelf3)
                                , (tblRef_S, set_SinRelf3)
                                , (tblRef_Eq, set_EqinRelf3)
                                , (tblRef_Element, set_ElementinRelf3)]
prov_relf3   = ProvInfo (Bimap.fromList 
                         [ (elm "e#0", consTerm "f#1")
                         , (elm "e#1", consTerm "someQf")
                         , (elm "e#2", Fn "f#7" [consTerm "f#1"]) 
                         , (elm "e#3", Fn "f#9" [consTerm "f#1"]) 
                         , (elm "e#4", Fn "someS" [ consTerm "f#1" ])
                         ])

thy_thyf4 = [ "Truth => exists someEB b . E(b) & B(b)"
            , "Truth => exists someED d . E(d) & D(d)"
            , "B(x) & D(y) => x = y" ]
list_EqClass1inf4 = [ elm "e#0", elm "e#1" ]
set_BinRelf4 = tableFromList [ [x] | x <- list_EqClass1inf4 ]
set_DinRelf4 = tableFromList [ [x] | x <- list_EqClass1inf4 ]
set_EinRelf4 = tableFromList [ [x] | x <- list_EqClass1inf4 ]
set_EqinRelf4 = tableFromList [ [x, y] | x <- list_EqClass1inf4 
                              , y <- list_EqClass1inf4]
set_ElementinRelf4 = tableFromList [ [x] | x <- list_EqClass1inf4 ]
base_relf4   = databaseFromList [ (tblRef_B, set_BinRelf4)
                                , (tblRef_D, set_DinRelf4)
                                , (tblRef_E, set_EinRelf4)
                                , (tblRef_Eq, set_EqinRelf4)
                                , (tblRef_Element, set_ElementinRelf4)]

prov_relf4   = ProvInfo (Bimap.fromList 
                         [ (elm "e#0", consTerm "someEB")
                         , (elm "e#1", consTerm "someED") ])
thy_thyf5 = [ "Truth => exists someB b . B(b)"
            , "Truth => exists someD d . D(d)"
            , "B(x) & D(y) => y=x" ]
list_EqClass1inf5 = [ elm "e#0", elm "e#1" ]
set_BinRelf5 = tableFromList [ [x] | x <- list_EqClass1inf5 ]
set_DinRelf5 = tableFromList [ [x] | x <- list_EqClass1inf5 ]
set_EqinRelf5 = tableFromList [ [x, y] | x <- list_EqClass1inf5 
                              , y <- list_EqClass1inf5]
set_ElementinRelf5 = tableFromList [ [x] | x <- list_EqClass1inf5 ]
base_relf5   = databaseFromList [ (tblRef_B, set_BinRelf5)
                                , (tblRef_D, set_DinRelf5)
                                , (tblRef_Eq, set_EqinRelf5)
                                , (tblRef_Element, set_ElementinRelf5)]
prov_relf5   = ProvInfo (Bimap.fromList 
                         [ (elm "e#0", consTerm "someB")
                         , (elm "e#1", consTerm "someD") ])

thy_thyf6 = [ "Truth => exists someB b . B(b)"
            , "Truth => exists someD d . D(d)"
            , "B(x) & D(y) => f(y) = f(x)" ]

list_EqClass1inf6 = [ elm "e#2", elm "e#3" ]
set_BinRelf6 = tableFromList [ [elm "e#0"] ]
set_DinRelf6 = tableFromList [ [elm "e#1"] ]
set_fnfinRelf6 = tableFromList $ 
                 [ [elm "e#0", x] | x <- list_EqClass1inf6 ] ++
                 [ [elm "e#1", x] | x <- list_EqClass1inf6 ]
set_EqinRelf6 = tableFromList $ 
                [ [elm "e#0", elm "e#0"], [elm "e#1", elm "e#1"]] ++
                [ [x, y] | x <- list_EqClass1inf6 
                , y <- list_EqClass1inf6]
set_ElementinRelf6 = tableFromList [ [elm "e#0"], [elm "e#1"]
                                   , [elm "e#2"], [elm "e#3"] ]
base_relf6   = databaseFromList [ (tblRef_B, set_BinRelf6)
                                , (tblRef_D, set_DinRelf6)
                                , (fnTblRef_f, set_fnfinRelf6)
                                , (tblRef_Eq, set_EqinRelf6)
                                , (tblRef_Element, set_ElementinRelf6)]
prov_relf6   = ProvInfo (Bimap.fromList 
                         [ (elm "e#0", consTerm "someB")
                         , (elm "e#1", consTerm "someD")
                         , (elm "e#2", Fn "f#3" [ consTerm "someB"
                                                , consTerm "someD"])
                         , (elm "e#3", Fn "f#1" [ consTerm "someB"
                                                , consTerm "someD"]) ] )

-- thy_thyf7 = [ "exists x. exists y.P(x,y)"
--             , "P(x,y) => f(x) = g(f(y))"
--             , "P(x,y) => Q(g(f(y)))"
--             , "~ Q(f(x))" ]

thy_thyf8 = [ "R(f(x)) => R(x)" ]

thy_thyf9 = [ "exists someElem x. exists anotherElem y.P(x,y)"
            , "P(x,y) => (Q(f(x),g(y)) | f(x) = g(y))"
            , "~ Q(x,y)" ]
list_EqClass1inRelf9 = [elm "e#2", elm "e#3"]
set_PinRelf9 = tableFromList [ [elm "e#0", elm "e#1"] ]
set_QinRelf9 = tableFromList [ [x, y] | x <- list_EqClass1inRelf9 
                             , y <- list_EqClass1inRelf9]
set_fnfinRelf9 = tableFromList [ [elm "e#0", x] | x <- list_EqClass1inRelf9 ]
set_fnginRelf9 = tableFromList [ [elm "e#1", x] | x <- list_EqClass1inRelf9 ]
set_EqinRelf9 = tableFromList $ 
                [ [elm "e#0", elm "e#0"], [elm "e#1", elm "e#1"]] ++
                [ [x, y] | x <- list_EqClass1inRelf9 
                , y <- list_EqClass1inRelf9]
set_ElementinRelf9 = tableFromList [ [elm "e#0"], [elm "e#1"]
                                   , [elm "e#2"], [elm "e#3"] ]
base_relf9   = databaseFromList [ (tblRef_P, set_PinRelf9)
                                , (tblRef_Q, set_QinRelf9)
                                , (fnTblRef_f, set_fnfinRelf9)
                                , (fnTblRef_g, set_fnginRelf9)
                                , (tblRef_Eq, set_EqinRelf9)
                                , (tblRef_Element, set_ElementinRelf9)]
prov_relf9   = ProvInfo (Bimap.fromList 
                         [ (elm "e#0", consTerm "someElem")
                         , (elm "e#1", consTerm "anotherElem")
                         , (elm "e#2", Fn "g#3" [ consTerm "someElem"
                                                , consTerm "anotherElem"])
                         , (elm "e#3", Fn "f#1" [ consTerm "someElem"
                                                , consTerm "anotherElem"]) ] )

thy_thyf10 = [ "exists somePf x.P(f(x))"
             , "exists someQf x.Q(f(x))"
             , "P(x) => Q(x)" ]
set_PinRelf10 = tableFromList [ [elm "e#0"] ]
set_QinRelf10 = tableFromList [ [elm "e#0"], [elm "e#2"] ]
set_fnfinRelf10 = tableFromList [ [elm "e#1", elm "e#0"] 
                                , [elm "e#3", elm "e#2"]]
set_EqinRelf10 = tableFromList [ [elm "e#0", elm "e#0"], [elm "e#1", elm "e#1"]
                               , [elm "e#2", elm "e#2"], [elm "e#3", elm "e#3"] ]
set_ElementinRelf10 = tableFromList [ [elm "e#0"], [elm "e#1"]
                                    , [elm "e#2"], [elm "e#3"] ]
base_relf10   = databaseFromList [ (tblRef_P, set_PinRelf10)
                                , (tblRef_Q, set_QinRelf10)
                                , (fnTblRef_f, set_fnfinRelf10)
                                , (tblRef_Eq, set_EqinRelf10)
                                , (tblRef_Element, set_ElementinRelf10)]
prov_relf10   = ProvInfo (Bimap.fromList 
                                 [ (elm "e#0", consTerm "f#1")
                                 , (elm "e#1", consTerm "somePf")
                                 , (elm "e#2", consTerm "f#3")
                                 , (elm "e#3", consTerm "someQf") ])

thy_thyf12 = [ "P(a())"
             , "P(a()) => Q(b())"
             , "Q(b()) => f(a()) = a()"
             , "P(f(x)) => Q(x)" ]
list_EqClass1inRelf12 = [elm "e#0", elm "e#2"]
set_ainRelf12 = tableFromList [ [elm "e#0"] ]
set_binRelf12 = tableFromList [ [elm "e#1"] ]
set_PinRelf12 = tableFromList [ [elm "e#0"], [elm "e#2"] ]
set_QinRelf12 = tableFromList [ [elm "e#0"], [elm "e#1"] , [elm "e#2"]]
set_fnfinRelf12 = tableFromList [ [elm "e#0", elm "e#0"] 
                                , [elm "e#0", elm "e#2"]
                                , [elm "e#2", elm "e#0"]
                                , [elm "e#2", elm "e#2"] ]
set_EqinRelf12 = tableFromList $ 
                 [ [elm "e#1", elm "e#1"] ] ++
                 [ [x, y] | x <- list_EqClass1inRelf12
                 , y <- list_EqClass1inRelf12 ] 
set_ElementinRelf12 = tableFromList [ [elm "e#0"], [elm "e#1"], [elm "e#2"] ]
base_relf12   = databaseFromList [ (tblRef_a, set_ainRelf12)
                                 , (tblRef_b, set_binRelf12)
                                 , (tblRef_P, set_PinRelf12)
                                 , (tblRef_Q, set_QinRelf12)
                                 , (fnTblRef_f, set_fnfinRelf12)
                                 , (tblRef_Eq, set_EqinRelf12)
                                 , (tblRef_Element, set_ElementinRelf12)]
prov_relf12   = ProvInfo (Bimap.fromList 
                                 [ (elm "e#0", consTerm "a")
                                 , (elm "e#1", consTerm "b")
                                 , (elm "e#2", consTerm "f#1") ])
