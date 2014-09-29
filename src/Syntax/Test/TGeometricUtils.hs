{- Razor
   Module      : Syntax.Test.TGeometricUtils
   Description : Unit tests for Term
   Maintainer  : Salman Saghafi -}

module Syntax.Test.TGeometricUtils where

-- Control
import Control.Monad.State.Lazy

-- HUnit
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Test
import Syntax.Test.TestData

-- Syntax
import Syntax.Term ( Variable (..), Constant (..), Element (..)
                   , Term (..), TermBased (..) )
import Syntax.IGeometricUtils
import Syntax.Geometric (parseFormula, parseSequent)


test_hasFreeHeadVar = 
    [ "test trivial sequent" ~:
      False ~=? hasFreeHeadVar seq_Triv
    , "test sequent with free variable in head and empty body" ~:
      True ~=? hasFreeHeadVar seq_Simple2
    , "test sequent without free variable in head" ~:
      False ~=? hasFreeHeadVar seq_Complex1
    , "test sequent without free and bound variable in head" ~:
      True ~=? hasFreeHeadVar seq_Complex4
    ]

test_freeVarsForAtom = 
    [ "test simple atom" ~:
      [var text_varX] ~=? freeVars atm_Px
    , "test simple atom with duplicate variables" ~:
      [var text_varX] ~=? freeVars atm_Pxcx
    , "test atom with complex terms" ~:
      [var text_varX, var text_varVar] ~=? freeVars atm_Complex2
    , "test atom with functional relation" ~:
      [var text_varX] ~=? freeVars atm_Fx
    , "test atom with functional relation with complex term" ~:
      [var text_varVar, var text_varX] ~=? freeVars atm_Complex3
    ]

test_constantsForAtom = 
    [ "test constants for simple atom" ~:
      [cons "c"] ~=? constants atm_Pc
    , "test constants for simple atom with duplicate constants" ~:
      [cons "c"] ~=? constants atm_Pcxc
    , "test constants for atom with complex terms" ~:
      [cons "c", cons "const"] ~=? constants atm_Complex2
    , "test constants for atom with functional relation" ~:
      [cons "c"] ~=? constants atm_Fc
    , "test constants for atom with functional relation with complex term" ~:
      [cons "const", cons "c"] ~=? constants atm_Complex3
    ]

test_freeVarsForFormula = 
    [ "test atomic formula" ~:
      [var text_varX, var text_varVar] ~=? freeVars fmla_atomComplex2
    , "test conjunction" ~:
      [var text_varX, var text_varY] ~=? freeVars fmla_PxAndQy
    , "test disjunction" ~:
      [var text_varX, var text_varY] ~=? freeVars fmla_PxOrQy
    , "test exists" ~:
      [] ~=? freeVars fmla_existsPx
    , "test exists with free variable" ~:
      [var text_varY] ~=? freeVars fmla_exists1
    ]

test_constantsForFormula = 
    [ "test constantsForFormula for atomic formula" ~:
      [cons "c", cons "const"] ~=? constants fmla_atomComplex2
    , "test constantsForFormula for conjunction" ~:
      [cons "a", cons "b"] ~=? constants fmla_PaAndQb
    , "test constantsForFormula for disjunction" ~:
      [cons "a", cons "b"] ~=? constants fmla_PaOrQb
    , "test constantsForFormula for exists" ~:
      [cons "a"] ~=? constants fmla_existsPxa
    ]

test_freeVarsForSequent = 
    [ "test trivial sequent" ~:
      [] ~=? freeVars seq_Triv
    , "test sequent with empty body" ~:
      [var text_varX] ~=? freeVars seq_Simple2
    , "test sequent with empty head" ~:
      [var text_varX] ~=? freeVars seq_Simple3
    , "test sequent with free variables on left and right" ~:
      [var text_varX, var text_varY] ~=? freeVars seq_Complex1
    , "test sequent for complex sequent" ~:
      [var text_varX, var text_varY] ~=? freeVars seq_Complex3
    ]

test_constantsForSequent = 
    [ "test constantsForSequent for trivial sequent" ~:
      [] ~=? constants seq_Triv
    , "test constantsForSequent for sequent with empty body" ~:
      [cons "a"] ~=? constants seq_Simple7
    , "test constantsForSequent for sequent with empty head" ~:
      [cons "a"] ~=? constants seq_Simple8
    , "test constantsForSequent for sequent with constants on left and right" ~:
      [cons "a", cons "b", cons "c"] ~=? constants seq_Complex5
    , "test constantsForSequent for complex sequent" ~:
      [cons "a", cons "b", cons "c"] ~=? constants seq_Complex6
    ]

test_functionSymsForAtom = 
    [ "test functionSymsForAtom for simple atom" ~:
      [] ~=? functionSyms atm_Px
    , "test functionSymsForAtom for simple atom with duplicate variables" ~:
      [("c", 0)] ~=? functionSyms atm_Pxcx
    , "test functionSymsForAtom for atom with complex terms" ~:
      [("c",0),("f",2),("const",0),("g",2),("f",3)] ~=? functionSyms atm_Complex2
    , "test functionSymsForAtom for atom with functional relation" ~:
      [] ~=? functionSyms atm_Fx
    , "test functionSymsForAtom for atom with complex term" ~:
      [("const", 0), ("c", 0), ("g", 2), ("f", 3)] ~=? functionSyms atm_Complex3
    ]


test_functionSymsForFormula = 
    [ "test functionSymsForFormula for atomic formula" ~:
      [("c",0),("f",2),("const",0),("g",2),("f",3)] ~=? 
      functionSyms fmla_atomComplex2
    , "test functionSymsForFormula for conjunction with no function symsbols" ~:
      [] ~=? functionSyms fmla_PxAndQy
    , "test functionSymsForFormula for conjunction with function symsbols" ~:
      [("c", 0), ("f", 2), ("g", 2)] ~=? functionSyms fmla_PfcxAndQxgyz
    , "test functionSymsForFormula for disjunction with no function symbols" ~:
      [] ~=? functionSyms fmla_PxOrQy
    , "test functionSymsForFormula for disjunction with no function symbols" ~:
      [("c", 0), ("f", 2), ("g", 2)] ~=? functionSyms fmla_PfcxOrQxgyz
    , "test functionSymsForFormula for exists with no function symbols" ~:
      [] ~=? functionSyms fmla_existsPx
    , "test functionSymsForFormula for exists with function symbols" ~:
      [("f", 2)] ~=? functionSyms fmla_existsQxfyz
    ]

test_functionSymsForSequent = 
    [ "test functionSymsForSequent for trivial sequent" ~:
      [] ~=? functionSyms seq_Triv
    , "test functionSymsForSequent with no function in body" ~:
      [("f", 1), ("g", 1)] ~=? functionSyms seq_seq2
    , "test functionSymsForSequent with no function in head" ~:
      [("f", 1)] ~=? functionSyms seq_seq5
    , "test functionSymsForSequent for sequent with function in head and body" ~:
      [("f", 1), ("c", 0), ("g", 2)] ~=? functionSyms seq_seq6
    ]

test_relationalizeSequent = 
    [ "test relationalizeSequent trivial sequent" ~:
      seq_Triv ~=? evalState (relationalizeSequent seq_Triv) 0
    , "test relationalizeSequent with no function symbols" ~:
      seq_Complex3_R ~=? evalState (relationalizeSequent seq_Complex3) 0
    , "test relationalizeSequent with function symbol in head" ~:
      seq_seq1_R ~=? evalState (relationalizeSequent seq_seq1) 0
    , "test relationalizeSequent with multiple function symbols in head" ~:
      seq_seq2_R ~=? evalState (relationalizeSequent seq_seq2) 0
    , "test relationalizeSequent with nullary function in head" ~:
      seq_seq3 ~=? evalState (relationalizeSequent seq_seq3) 0
    , "test relationalizeSequent with constant in head" ~:
      seq_seq4 ~=? evalState (relationalizeSequent seq_seq4) 0
    , "test relationalizeSequent with function in body" ~:
      seq_seq5_R ~=? evalState (relationalizeSequent seq_seq5) 0
    ]

test_integritySequents = 
    [ "test nullary function" ~:
      seq_integ0 ~=? integritySequents ("f", 0)
    , "test function of arity 1" ~:
      seq_integ1 ~=? integritySequents ("f", 1)
    , "test function of arity 2" ~:
      seq_integ2 ~=? integritySequents ("f", 2)
    , "test function of arity 3" ~:
      seq_integ3 ~=? integritySequents ("f", 3)
    ]

test_addElementPreds = 
    [ "test trivial sequent" ~:
      seq_Triv ~=? addElementPreds seq_Triv
    , "test sequent with no free and bound variables in head" ~:
      seq_Simple1 ~=? addElementPreds seq_Simple1
    , "test sequent with empty body and free variable in head" ~:
      seq_Simple2_El ~=? addElementPreds seq_Simple2
    , "test sequent with free variable in head" ~:
      seq_Simple4_El ~=? addElementPreds seq_Simple4
    , "test sequent with bound variable in head" ~:
      seq_Simple5_El ~=? addElementPreds seq_Simple5
    , "test sequent with free and bound variables in head" ~:
      seq_Simple6_El ~=? addElementPreds seq_Simple6
    ]

test_relationSymsForAtom = 
    [ "test relationSymsForAtom for nullary atom" ~:
      [("P", 0)] ~=? relationSyms atm_P
    , "test relationSymsForAtom for simple atom" ~:
      [("P", 3)] ~=? relationSyms atm_Pxcx
    , "test relationSymsForAtom for atom with complex terms" ~:
      [("R",2)] ~=? relationSyms atm_Complex2
    , "test relationSymsForAtom for atom with functional relation" ~:
      [("F", 1)] ~=? relationSyms atm_Fx
    ]

test_relationSymsForFormula = 
    [ "test relationSymsForFormula for atomic formula" ~:
      [("R",2)] ~=? relationSyms fmla_atomComplex2
    , "test relationSymsForFormula for conjunction" ~:
      [("P", 1), ("Q", 2)] ~=? relationSyms fmla_PfcxAndQxgyz
    , "test relationSymsForFormula for disjunction" ~:
      [("P", 1), ("Q", 2)] ~=? relationSyms fmla_PfcxAndQxgyz
    , "test relationSymsForFormula for exists" ~:
      [("Q", 2)] ~=? relationSyms fmla_existsQxfyz
    ]

test_relationSymsForSequent = 
    [ "test relationSymsForSequent for trivial sequent" ~:
      [] ~=? relationSyms seq_Triv
    , "test relationSymsForSequent with empty body" ~:
      [("P", 1)] ~=? relationSyms seq_Simple2
    , "test relationSymsForSequent with empty head" ~:
      [("P", 1)] ~=? relationSyms seq_Simple3
    , "test relationSymsForSequent for sequent with relation in head and body" ~:
      [("P", 1), ("Q", 1)] ~=? relationSyms seq_seq6
    ]

test_simplify =    
    [ "test simplify for conjunction with Falsehood on left" ~:
      fmla_falsehood ~=? simplify (parseFormula "Falsehood & P(x)")
    , "test simplify for conjunction with Falsehood on right" ~:
      fmla_falsehood ~=? simplify (parseFormula "P(x) & Falsehood")
    , "test simplify for conjunction with Truth on left" ~:
      parseFormula "P(x)" ~=? simplify (parseFormula "Truth & P(x)")
    , "test simplify for conjunction with Truth on right" ~:
      parseFormula "P(x)" ~=? simplify (parseFormula "P(x) & Truth")
    , "test simplify for disjunction with Truth on left" ~:
      fmla_truth ~=? simplify (parseFormula "Truth | P(x)")
    , "test simplify for disjunction with Truth on right" ~:
      fmla_truth ~=? simplify (parseFormula "P(x) | Truth")
    , "test simplify for disjunction with Falsehood on left" ~:
      parseFormula "P(x)" ~=? simplify (parseFormula "Falsehood | P(x)")
    , "test simplify for disjunction with Falsehood on right" ~:
      parseFormula "P(x)" ~=? simplify (parseFormula "P(x) | Falsehood")
    , "test simplify for exists with relevant variable" ~:
      parseFormula "exists x. P(x)" ~=? simplify (parseFormula "exists x. P(x)")
    , "test simplify for exists with irrelevant variable" ~:
      parseFormula "Q(x)" ~=? simplify (parseFormula "exists y. Q(x)")
    , "test simplify for formula that cannot be simplified" ~:
      parseFormula "exists x. P(x) | Q(y)" ~=? 
      simplify (parseFormula "exists x. P(x) | Q(y)")
    , "test simplify for conjunction and disjunction" ~:
      parseFormula "P(x)" ~=? simplify (parseFormula "P(x) & (Q(y) | Truth)")
    , "test simplify for disjunction and conjunction" ~:
      parseFormula "Q(x)" ~=? simplify (parseFormula "Q(x) | (P(y) & Falsehood)")
    , "test simplify for exists, disjunction and conjunction" ~:
      parseFormula "Q(x)" ~=? 
      simplify (parseFormula "exists y z. Q(x) | (P(y) & Falsehood)")
    ]

test_formulaExistentials =
    [ "test formulaExistentials for atomic formula" ~:
      [] ~=? formulaExistentials fmla_atomComplex2
    , "test formulaExistentials for exists without Skolem function" ~:
      [] ~=? formulaExistentials fmla_existsPx
    , "test formulaExistentials for exists with Skolem function" ~:
      ["exists#0"] ~=? formulaExistentials fmla_existsPx_R
    , "test formulaExistentials for cascade exists" ~:
      ["sk1", "sk2"] ~=? formulaExistentials fmla_exists3
    , "test formulaExistentials for conjunction" ~:
      ["sk1", "sk2"] ~=? formulaExistentials fmla_exists4
    , "test formulaExistentials for disjunction" ~:
      ["sk1"] ~=? formulaExistentials fmla_exists5
    ]

test_sequentExistentials = 
    [ "test sequentExistentials for trivial sequent" ~:
      [] ~=? sequentExistentials seq_Triv
    , "test sequentExistentials for sequent with empty body" ~:
      ["sk1"] ~=? sequentExistentials seq_seqExists1
    , "test sequentExistentials for sequent with empty head" ~:
      ["sk1"] ~=? sequentExistentials seq_seqExists2
    , "test sequentExistentials for regular sequent" ~:
      ["sk1", "sk2", "sk3"] ~=? sequentExistentials seq_seqExists3
    ]

test_formulaExistsSubstitute =
    [ "test formulaExistsSubstitute for atomic formula" ~:
      fmla_atomComplex2 ~=? 
      formulaExistsSubstitute exSub_sk1Toa fmla_atomComplex2
    , "test formulaExistsSubstitute for exists without Skolem function" ~:
      fmla_existsPx ~=? formulaExistsSubstitute exSub_sk1Toa fmla_existsPx
    , "test formulaExistsSubstitute for exists with no matching substitution" ~:
      fmla_existsPx_R ~=? formulaExistsSubstitute exSub_sk1Toa fmla_existsPx_R
    , "test formulaExistsSubstitute for exists with matching substitution" ~:
      (parseFormula "P('a)") ~=? 
      formulaExistsSubstitute exSub_sk1Toa fmla_exists6
    , "test formulaExistsSubstitute for cascade exists matching one" ~:
      (parseFormula "exists sk2 y . P('a, y)") ~=? 
      formulaExistsSubstitute exSub_sk1Toa fmla_exists3
    , "test formulaExistsSubstitute for cascade exists matching all" ~:
      (parseFormula "P('a, 'b)") ~=? 
      formulaExistsSubstitute exSub_sk1Toask2Tob fmla_exists3
    , "test formulaExistsSubstitute for conjunction" ~:
      (parseFormula "P('a) & Q(x, 'b)") ~=? 
      formulaExistsSubstitute exSub_sk1Toask2Tob fmla_exists4
    , "test formulaExistsSubstitute for disjunction" ~:
      (parseFormula "P(z) | Q(x, 'a)") ~=? 
      formulaExistsSubstitute exSub_sk1Toask2Tob fmla_exists5
    ]
    
test_sequentExistsSubstitute = 
    [ "test sequentExistsSubstitute for trivial sequent" ~:
      seq_Triv ~=? sequentExistsSubstitute exSub_sk1Toa seq_Triv
    , "test sequentExistsSubstitute for sequent with empty body" ~:
      (parseSequent "P('a)") ~=? 
      sequentExistsSubstitute exSub_sk1Toa seq_seqExists1
    , "test sequentExistsSubstitute for sequent with empty head" ~:
      (parseSequent "P('a) => Falsehood") ~=? 
      sequentExistsSubstitute exSub_sk1Toa seq_seqExists2
    , "test sequentExistsSubstitute for regular sequent" ~:
      (parseSequent "P('a) => exists sk3 y . Q('b, y)") ~=? 
      sequentExistsSubstitute exSub_sk1Toask2Tob seq_seqExists3
    ]


test_all = test_hasFreeHeadVar
           ++ test_freeVarsForAtom ++ test_constantsForAtom
           ++ test_freeVarsForFormula ++ test_constantsForFormula
           ++ test_freeVarsForSequent ++ test_constantsForSequent
           ++ test_functionSymsForAtom ++ test_functionSymsForFormula 
           ++ test_functionSymsForSequent
           ++ test_relationalizeSequent ++ test_integritySequents
           ++ test_addElementPreds 
           ++ test_relationSymsForAtom ++ test_relationSymsForFormula 
           ++ test_relationSymsForSequent
           ++ test_simplify
           ++ test_formulaExistentials ++ test_sequentExistentials
           ++ test_formulaExistsSubstitute ++ test_sequentExistsSubstitute