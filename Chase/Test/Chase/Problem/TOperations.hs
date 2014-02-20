module Chase.Test.Chase.Problem.TOperations where


import Chase.Test.HUnit
import Chase.Test.HUnit.Text (runTestTT)

-- unit under test:
import Chase.Formula.SyntaxGeo (Vars)
import Chase.Problem.IOperations

import Chase.Test.TestData
import Chase.Test.TestHelper

import Chase.Problem.Model (Model (..))
import Chase.Problem.Structures (ID(..), 
                                 Frame(Frame), 
                                 Problem(Problem))

import qualified Data.Set as Set
import Debug.Trace
---------------------------------------------------
test_processHead = 
    ["test Fls" ~: [] ~=? (processHead fmlaFls)
    , "test Atom" ~: [[obs1]] ~=? (processHead fmla1)
    , "test And" ~: [[obs1, obs2]] ~=? (processHead fmla2)
    , "test Or" ~: [[obs1], [obs2]] ~=? (processHead fmla3)
    , "test Exists" ~: [[obs1]] ~=? (processHead fmla4)
    , "test =" ~: [[obs4]] ~=? (processHead fmla7)
    , "test combined" ~: 
     [[obs1, obs2], [obs3, obs2]] ~=? (processHead fmla5)]

---------------------------------------------------
test_processBody = 
    ["test Tru" ~: [] ~=? (processBody fmlaTru)
    , "test Atom" ~: [obs1] ~=? (processBody fmla1)
    , "test And" ~: [obs1, obs2] ~=? (processBody fmla2)
    , "test =" ~: [obs4] ~=? (processBody fmla7)
    , "test combined" ~: [obs4, obs5] ~=? (processBody fmla8)]
---------------------------------------------------
test_buildFrame = 
    [TestCase $ assertBool "test empty sequent" 
    $ equalFrames frm1  $ buildFrame (ID 1) sqnt1
    , TestCase $ assertBool "test bodyless sequent 1" 
    $ equalFrames frm2 $ buildFrame (ID 2) sqnt2
    , TestCase $ assertBool "test bodyless sequent 2" 
    $ equalFrames frm8 $ buildFrame (ID 8) sqnt8
    , TestCase $ assertBool "test headless sequent" 
    $ equalFrames frm3 $ buildFrame (ID 3) sqnt3
    , TestCase $ assertBool "test sequent with no vars" 
    $ equalFrames frm4 $ buildFrame (ID 4) sqnt4
    , TestCase $ assertBool "test random sequent 1" 
    $ equalFrames frm5 $ buildFrame (ID 5) sqnt5
    , TestCase $ assertBool "test random sequent 2" 
    $ equalFrames frm6 $ buildFrame (ID 6) sqnt6
    , TestCase $ assertBool "test random sequent 3" 
    $ equalFrames frm7 $ buildFrame (ID 7) sqnt7
    , TestCase $ assertBool "test random sequent 4" 
    $ equalFrames frm8 $ buildFrame (ID 8) sqnt8
    , TestCase $ assertBool "test random sequent 5" 
    $ equalFrames frm9 $ buildFrame (ID 9) sqnt9]

---------------------------------------------------
test_framesSymbolMap = 
    ["test thyf12" ~: symMap1 ~=? (framesSymbolMap frmlst1)
    , "test single empty sequent" ~: 
     symMap2 ~=? (framesSymbolMap frmlst2)
    , "test random frames" ~: 
     symMap3 ~=? (framesSymbolMap frmlst3)
    , "test random frames" ~:
     symMap2 ~=? (framesSymbolMap frmlst4)]
---------------------------------------------------
test_buildProblem = 
    [TestCase $ assertBool "test thyf12" 
    $ equalProblems prob1 $ buildProblem thy1
    , TestCase $ assertBool "test thy0" 
    $ equalProblems prob2 $ buildProblem thy4
    , TestCase $ assertBool "test thy7" 
    $ equalProblems prob3 $ buildProblem thy5]
---------------------------------------------------
test_emptyQueue = 
    ["test empty queue" ~: True ~=? (emptyQueue prob1)
    , "non empty queue" ~: False ~=? (emptyQueue prob1_1)]
---------------------------------------------------
test_all = test_processHead ++ test_processBody ++ test_buildFrame ++
           test_framesSymbolMap ++ test_buildProblem ++ test_emptyQueue
---------------------------------------------------
tests = TestList test_all
---------------------------------------------------