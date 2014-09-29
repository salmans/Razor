{- Razor
   Module      : Syntax.Test.TFirstOrderUtils
   Description : Unit tests for Term
   Maintainer  : Salman Saghafi -}

module Syntax.Test.TFirstOrderUtils where

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
import Syntax.IFirstOrderUtils
import Syntax.FirstOrder (parseFormula)

test_freeVarsForAtom = 
    [ "test simple atom" ~:
      [var text_varX] ~=? freeVars foa_Px
    , "test simple atom with duplicate variables" ~:
      [var text_varX] ~=? freeVars foa_Pxcx
    , "test atom with complex terms" ~:
      [var text_varX, var text_varVar] ~=? freeVars foa_Complex2
    , "test atom with functional relation" ~:
      [var text_varX] ~=? freeVars foa_Fx
    , "test atom with functional relation with complex term" ~:
      [var text_varVar, var text_varX] ~=? freeVars foa_Complex3
    ]

test_constantsForAtom = 
    [ "test constants for simple atom" ~:
      [cons "c"] ~=? constants foa_Pc
    , "test constants for simple atom with duplicate constants" ~:
      [cons "c"] ~=? constants foa_Pcxc
    , "test constants for atom with complex terms" ~:
      [cons "c", cons "const"] ~=? constants foa_Complex2
    , "test constants for atom with functional relation" ~:
      [cons "c"] ~=? constants foa_Fc
    , "test constants for atom with functional relation with complex term" ~:
      [cons "const", cons "c"] ~=? constants foa_Complex3
    ]

test_freeVarsForFormula = 
    [ "test atomic formula" ~:
      [var text_varX, var text_varVar] ~=? freeVars fof_atomComplex2
    , "test conjunction" ~:
      [var text_varX, var text_varY] ~=? freeVars fof_PxAndQy
    , "test disjunction" ~:
      [var text_varX, var text_varY] ~=? freeVars fof_PxOrQy
    , "test conjunction" ~:
      [var text_varX, var text_varY] ~=? freeVars fof_PxImpQy
    , "test disjunction" ~:
      [var text_varX, var text_varY] ~=? freeVars fof_PxIffQy
    , "test exists" ~:
      [] ~=? freeVars fof_existsPx
    , "test exists with free variable" ~:
      [var text_varY] ~=? freeVars fof_exists1
    ]

test_constantsForFormula = 
    [ "test constantsForFormula for atomic formula" ~:
      [cons "c", cons "const"] ~=? constants fof_atomComplex2
    , "test constantsForFormula for conjunction" ~:
      [cons "a", cons "b"] ~=? constants fof_PaAndQb
    , "test constantsForFormula for disjunction" ~:
      [cons "a", cons "b"] ~=? constants fof_PaOrQb
    , "test constantsForFormula for exists" ~:
      [cons "a"] ~=? constants fof_existsPxa
    ]

test_functionSymsForAtom = 
    [ "test functionSymsForAtom for simple atom" ~:
      [] ~=? functionSyms foa_Px
    , "test functionSymsForAtom for simple atom with duplicate variables" ~:
      [("c", 0)] ~=? functionSyms foa_Pxcx
    , "test functionSymsForAtom for atom with complex terms" ~:
      [("c",0),("f",2),("const",0),("g",2),("f",3)] ~=? functionSyms foa_Complex2
    , "test functionSymsForAtom for atom with functional relation" ~:
      [] ~=? functionSyms foa_Fx
    , "test functionSymsForAtom for atom with complex term" ~:
      [("const", 0), ("c", 0), ("g", 2), ("f", 3)] ~=? functionSyms foa_Complex3
    ]


test_functionSymsForFormula = 
    [ "test functionSymsForFormula for atomic formula" ~:
      [("c",0),("f",2),("const",0),("g",2),("f",3)] ~=? 
      functionSyms fof_atomComplex2
    , "test functionSymsForFormula for conjunction with no function symsbols" ~:
      [] ~=? functionSyms fof_PxAndQy
    , "test functionSymsForFormula for conjunction with function symsbols" ~:
      [("c", 0), ("f", 2), ("g", 2)] ~=? functionSyms fof_PfcxAndQxgyz
    , "test functionSymsForFormula for disjunction with no function symbols" ~:
      [] ~=? functionSyms fof_PxOrQy
    , "test functionSymsForFormula for disjunction with function symbols" ~:
      [("c", 0), ("f", 2), ("g", 2)] ~=? functionSyms fof_PfcxOrQxgyz
    , "test functionSymsForFormula for implication with no function symsbols" ~:
      [] ~=? functionSyms fof_PxImpQy
    , "test functionSymsForFormula for implication with function symsbols" ~:
      [("c", 0), ("f", 2), ("g", 2)] ~=? functionSyms fof_PfcxImpQxgyz
    , "test functionSymsForFormula for iff with no function symbols" ~:
      [] ~=? functionSyms fof_PxIffQy
    , "test functionSymsForFormula for iff with function symbols" ~:
      [("c", 0), ("f", 2), ("g", 2)] ~=? functionSyms fof_PfcxIffQxgyz
    , "test functionSymsForFormula for exists with no function symbols" ~:
      [] ~=? functionSyms fof_existsPx
    , "test functionSymsForFormula for exists with function symbols" ~:
      [("f", 2)] ~=? functionSyms fof_existsQxfyz
    ]

test_relationSymsForAtom = 
    [ "test relationSymsForAtom for nullary atom" ~:
      [("P", 0)] ~=? relationSyms foa_P
    , "test relationSymsForAtom for simple atom" ~:
      [("P", 3)] ~=? relationSyms foa_Pxcx
    , "test relationSymsForAtom for atom with complex terms" ~:
      [("R",2)] ~=? relationSyms foa_Complex2
    , "test relationSymsForAtom for atom with functional relation" ~:
      [("F", 1)] ~=? relationSyms foa_Fx
    ]

test_relationSymsForFormula = 
    [ "test relationSymsForFormula for atomic formula" ~:
      [("R",2)] ~=? relationSyms fof_atomComplex2
    , "test relationSymsForFormula for conjunction" ~:
      [("P", 1), ("Q", 2)] ~=? relationSyms fof_PfcxAndQxgyz
    , "test relationSymsForFormula for disjunction" ~:
      [("P", 1), ("Q", 2)] ~=? relationSyms fof_PfcxAndQxgyz
    , "test relationSymsForFormula for exists" ~:
      [("Q", 2)] ~=? relationSyms fof_existsQxfyz
    ]

test_simplify =    
    [ "test simplify for not True" ~:
      fof_falsehood ~=? simplify (parseFormula "~Truth")
    , "test simplify for not False" ~:
      fof_truth ~=? simplify (parseFormula "~Falsehood")

    , "test simplify for conjunction with Falsehood on left" ~:
      fof_falsehood ~=? simplify (parseFormula "Falsehood & P(x)")
    , "test simplify for conjunction with Falsehood on right" ~:
      fof_falsehood ~=? simplify (parseFormula "P(x) & Falsehood")
    , "test simplify for conjunction with Truth on left" ~:
      parseFormula "P(x)" ~=? simplify (parseFormula "Truth & P(x)")
    , "test simplify for conjunction with Truth on right" ~:
      parseFormula "P(x)" ~=? simplify (parseFormula "P(x) & Truth")
    , "test simplify for disjunction with Truth on left" ~:
      fof_truth ~=? simplify (parseFormula "Truth | P(x)")
    , "test simplify for disjunction with Truth on right" ~:
      fof_truth ~=? simplify (parseFormula "P(x) | Truth")
    , "test simplify for disjunction with Falsehood on left" ~:
      parseFormula "P(x)" ~=? simplify (parseFormula "Falsehood | P(x)")
    , "test simplify for disjunction with Falsehood on right" ~:
      parseFormula "P(x)" ~=? simplify (parseFormula "P(x) | Falsehood")

    , "test simplify for implication with Truth on left" ~:
      parseFormula "P(x)" ~=? simplify (parseFormula "Truth => P(x)")
    , "test simplify for implication with Truth on right" ~:
      fof_truth ~=? simplify (parseFormula "P(x) => Truth")
    , "test simplify for implication with Falsehood on left" ~:
      fof_truth ~=? simplify (parseFormula "Falsehood => P(x)")
    , "test simplify for implication with Falsehood on right" ~:
      parseFormula "~P(x)" ~=? simplify (parseFormula "P(x) => Falsehood")

    , "test simplify for bi-implication with Truth on left" ~:
      parseFormula "P(x)" ~=? simplify (parseFormula "Truth <=> P(x)")
    , "test simplify for bi-implication with Truth on right" ~:
      parseFormula "P(x)" ~=? simplify (parseFormula "P(x) <=> Truth")
    , "test simplify for bi-implication with Falsehood on left" ~:
      parseFormula "~P(x)" ~=? simplify (parseFormula "Falsehood <=> P(x)")
    , "test simplify for bi-implication with Falsehood on right" ~:
      parseFormula "~P(x)" ~=? simplify (parseFormula "P(x) <=> Falsehood")

    , "test simplify for exists with relevant variable" ~:
      parseFormula "exists x. P(x)" ~=? simplify (parseFormula "exists x. P(x)")
    , "test simplify for exists with irrelevant variable" ~:
      parseFormula "Q(x)" ~=? simplify (parseFormula "exists y. Q(x)")

    , "test simplify for forall with relevant variable" ~:
      parseFormula "forall x. P(x)" ~=? simplify (parseFormula "forall x. P(x)")
    , "test simplify for forall with irrelevant variable" ~:
      parseFormula "Q(x)" ~=? simplify (parseFormula "forall y. Q(x)")

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

test_all = test_freeVarsForAtom ++ test_constantsForAtom
           ++ test_freeVarsForFormula  ++ test_constantsForFormula
           ++ test_functionSymsForAtom ++ test_functionSymsForFormula
           ++ test_relationSymsForAtom ++ test_relationSymsForFormula
           ++ test_simplify