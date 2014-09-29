{- Razor
   Module      : Syntax.Test.TTerm
   Description : Unit tests for Term
   Maintainer  : Salman Saghafi -}

module Syntax.Test.TTerm where

-- Control
import Control.Monad.State.Lazy

-- HUnit
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Test
import Syntax.Test.TestData

-- Syntax
import Syntax.ITerm

test_parseTerm =
    ["test parseTerm for a variable" ~:
     term_varX ~=? (parseTerm text_varX),

     "test parseTerm for a variable with multiple characters" ~:
     term_varVar ~=? (parseTerm text_varVar),

     "test parseTerm for a constant" ~:
     term_consC ~=? (parseTerm text_consC),

     "test parseTerm for a constant with multiple characters" ~:
     term_consConst ~=? (parseTerm text_consConst),

     "test parseTerm for a function applied to arguments" ~:
     term_termSimple1 ~=? (parseTerm text_termSimple1),

     "test parseTerm for a complex term" ~:
     term_termComplex1 ~=? (parseTerm text_termComplex1)
    ]

test_freeVarsForTerm = 
    [ "test variable" ~:
      [var text_varX] ~=? freeVars term_varX
    , "test constant" ~:
      [] ~=? freeVars term_consC
    , "test element" ~:
      [] ~=? freeVars term_elmE
    , "test nullary function" ~:
      [] ~=? freeVars term_fnF
    , "test function without variables" ~:
      [] ~=? freeVars term_fnFbc
    , "test function with variable" ~:
      [var text_varX] ~=? freeVars term_fnFxc
    , "test function with duplicate variable" ~:
      [var text_varX, var text_varY] ~=? freeVars term_fnFxcxy
    , "test function with function inside" ~:
      [var text_varVar, var text_varX] ~=? freeVars term_termComplex1
    ]

test_constantsForTerm = 
    [ "test constants for variable" ~:
      [] ~=? constants term_varX
    , "test constants for constant" ~:
      [cons "c"] ~=? constants term_consC
    , "test constants for element" ~:
      [] ~=? constants term_elmE
    , "test constants for nullary function" ~:
      [cons "f"] ~=? constants term_fnF
    , "test constants for function without variables" ~:
      [cons "b", cons "c"] ~=? constants term_fnFbc
    , "test constans for function with variable" ~:
      [cons "c"] ~=? constants term_fnFxc
    , "test constants function with duplicate constants" ~:
      [cons "b", cons "c"] ~=? constants term_fnFbxbc
    , "test constants for function with function inside" ~:
      [cons "const", cons "c"] ~=? constants term_termComplex1
    ]

test_functionSymsForTerm =
    [ "test functionSymsForTerm for variable" ~:
      [] ~=? functionSyms term_varX
    , "test functionSymsForTerm for constant" ~:
      [("c", 0)] ~=? functionSyms term_consC
    , "test functionSymsForTerm for element" ~:
      [] ~=? freeVars term_elmE
    , "test functionSymsForTerm for nullary function" ~:
      [("f",0)] ~=? functionSyms term_fnF
    , "test functionSymsForTerm for function without variables" ~:
      [("b", 0), ("c", 0), ("f", 2)] ~=? functionSyms term_fnFbc
    , "test functionSymsForTerm for function with variable" ~:
      [("c", 0), ("f", 2)] ~=? functionSyms term_fnFxc
    , "test functionSymsForTerm for function with duplicate variable" ~:
      [("c", 0), ("f", 4)] ~=? functionSyms term_fnFxcxy
    , "test functionSymsForTerm for function with function inside" ~:
      [("const", 0), ("c", 0), ("g", 2), ("f", 3)] ~=? 
      functionSyms term_termComplex1
    ]

test_freshVariable = [ "test freshVariable" ~:
                       (Variable "v#1") ~=? evalState (freshVariable) 1 
                     , "test calling freshVariable twice" ~:
                       (Variable "v#2") ~=? 
                       evalState (freshVariable >> freshVariable) 1 ]

test_freshConstant = [ "test freshConstant" ~:
                       (Constant "c#1") ~=? evalState (freshConstant) 1 
                     , "test calling freshConstant twice" ~:
                       (Constant "c#2") ~=? 
                       evalState (freshConstant >> freshConstant) 1 ]

test_freshElement  = [ "test freshElement" ~:
                       (Element "e#1") ~=? evalState (freshElement) 1 
                     , "test calling freshElement twice" ~:
                       (Element "e#2") ~=? 
                       evalState (freshElement >> freshElement) 1 ]

test_isConstant    = [ "test isConstant for a constant" ~:
                       True  ~=? isConstant term_consC
                     , "test isConstant for a variable" ~:
                       False ~=? isConstant term_varY
                     , "test isConstant for an element" ~:
                       False ~=? isConstant (Elem (Element "e"))
                     , "test isConstant for a function application" ~:
                       False ~=? isConstant term_termSimple1
                     ]

test_isVariable    = [ "test isVariable for a variable" ~:
                       True ~=? isVariable term_varY
                     , "test isVariable for a constant" ~:
                       False  ~=? isVariable term_consC
                     , "test isConstant for an element" ~:
                       False ~=? isVariable (Elem (Element "e"))
                     , "test isConstant for a function application" ~:
                       False ~=? isVariable term_termSimple1
                     ]

test_termToVariable = 
    [ "test termToVariable for a variable" ~:
      Just (Variable "y") ~=? termToVariable term_varY
    , "test termToVariable for a constant" ~:
      Nothing  ~=? termToVariable term_consC
    , "test termToVariable for an element" ~:
      Nothing ~=? termToVariable (Elem (Element "e"))
    , "test termToVaribale for a function application" ~:
      Nothing ~=? termToVariable term_termSimple1
    ]

test_termToConstant = 
    [ "test termToConstant for a constant" ~:
      Just (Constant "c")  ~=? termToConstant term_consC
    , "test termToConstant for a variable" ~:
      Nothing  ~=? termToConstant term_varY
    , "test termToConstant for an element" ~:
      Nothing ~=? termToConstant (Elem (Element "e"))
    , "test termToConstant for a function application" ~:
      Nothing ~=? termToConstant term_termSimple1
    , "test termToConstant for a nullary function" ~:
      Just (Constant "c") ~=? termToConstant (Fn "c" [])
    ]

test_variant =
    [ "test variant for empty list" ~:
      Variable "x" ~=? variant (Variable "x") []
    , "test variant for a non-existing variable" ~:
      Variable "x" ~=? variant (Variable "x") [(Variable "y"), (Variable "z")]
    , "test variant for an existing variable" ~:
      Variable "x'" ~=? variant (Variable "x") [(Variable "z"), (Variable "x")]
    , "test variant for a variable that exists multiple times" ~:
      Variable "x'" ~=? variant (Variable "x") [(Variable "x"), (Variable "x")]
    , "test variant for a variable whose variants exist" ~:
      Variable "x'''" ~=? 
               variant (Variable "x") 
                       [(Variable "x'"), (Variable "x"), (Variable "x''")]
    ]

test_termDepth = 
    [ "test termDepth for variable" ~:
      0 ~=? termDepth term_varX
    , "test termDepth for constant" ~:
      0 ~=? termDepth term_consC
    , "test termDepth for element" ~:
      0 ~=? termDepth term_elmE
    , "test termDepth for nullary function" ~:
      0 ~=? termDepth term_fnF
    , "test termDepth for function without variables" ~:
      1 ~=? termDepth term_fnFbc
    , "test termDepth for function with variable" ~:
      1 ~=? termDepth term_fnFxc
    , "test termDepth for function with function inside" ~:
      2 ~=? termDepth term_termComplex1
    ]

test_all = test_parseTerm ++ test_freeVarsForTerm ++ test_constantsForTerm
           ++ test_functionSymsForTerm
           ++ test_freshVariable ++ test_freshConstant ++ test_freshElement
           ++ test_isVariable ++ test_isConstant
           ++ test_termToVariable ++ test_termToConstant ++ test_variant
           ++ test_termDepth