module Chase.Test.Chase.TChase where


import Chase.Test.HUnit
import Chase.Test.HUnit.Text (runTestTT)

-- unit under test:
import Chase.Formula.SyntaxGeo

import Chase.Test.TestData
import Chase.Test.TestHelper

import Chase.Problem.Structures (ID(..), 
                                 Frame(Frame), 
                                 Problem(Problem))
import Chase.IChase
import Debug.Trace

---------------------------------------------------
{- The tests in this category also tests deduceForFrameHelper and deduce. -}
test_deduceForFrame = 
    ["test frames with body on empty model" ~: 
     ([], 0) ~=? (deduceForFrame 0 mdl0 frm4),
     "test frames with body on non-empty model" ~: 
     ([], 0) ~=? (deduceForFrame 0 mdl1 frm4),
     "test fact is already true" ~: 
     ([], 0) ~=? (deduceForFrame 0 mdl1 frm8),
     "test equation is already true" ~: 
     ([], 0) ~=? (deduceForFrame 0 mdl5 frm15),
     TestCase $ assertBool "test deduce equality"
                  ((equalLists l5 [[obs7]]) && c5 == 0),
     TestCase $ assertBool "test deduce fact"
                  ((equalLists l6 [[obs6]]) && c6 == 0),
     TestCase $ assertBool "test conjunction"
                  ((equalLists l7 [[obs5, obs6]]) && c7 == 0),
     TestCase $ assertBool "test disjunction"
                  ((equalLists l8 [[obs5], [obs6]]) && c8 == 0),
     "test existential already true" ~: 
     ([], 0) ~=? (deduceForFrame 0 mdl1 frm19),
     TestCase $ assertBool "test existential"
                  ((equalLists l10 [[obs24]]) && c10 == 1),
     TestCase $ assertBool "test multiple conjuncted existentials"
                  ((equalLists l11 [[obs24, obs26]]) && c11 == 1),
     TestCase $ assertBool "test multiple disjuncted existentials"
                  ((equalLists l12 [[obs24], [obs27]]) && c12 == 2),
     TestCase $ assertBool "test complex 1"
                  ((equalLists l13 [[obs22, obs6]]) && 
                   c13 == 0)]
    where (l5, c5) = deduceForFrame 0 mdl1 frm15
          (l6, c6) = deduceForFrame 0 mdl1 frm17
          (l7, c7) = deduceForFrame 0 mdl0 frm16
          (l8, c8) = deduceForFrame 0 mdl0 frm18
          (l10, c10) = deduceForFrame 0 mdl0 frm19
          (l11, c11) = deduceForFrame 0 mdl0 frm20
          (l12, c12) = deduceForFrame 0 mdl0 frm21
          (l13, c13) = deduceForFrame 0 mdl1 frm22
---------------------------------------------------
test_instFrame = 
    ["test empty frame" ~: [] ~=? (instFrame mdl0 symMap1 obs0 frm1),
     -- it must not return any frames for frames with no free variables:
     "test closed frame 1" ~: [] ~=? (instFrame mdl0 symMap1 obs0 frm8),
     "test closed frame 2" ~: [] ~=? (instFrame mdl0 symMap1 obs0 frm4),
     "test closed frame 3" ~: [] ~=? (instFrame mdl0 symMap1 obs0 frm6),
     "test closed frame with existentials" ~: 
     [] ~=? (instFrame mdl0 symMap1 obs0 frm10),
     (trace (show obs5))
     (trace (show frm2))
     (trace (show frm8))
     (trace (show (instFrame mdl0 symMap3 obs5 frm2)))
     "test fact observation" ~: [frm8] ~=? (instFrame mdl0 symMap3 obs5 frm2)]

---------------------------------------------------
test_all = test_deduceForFrame ++ test_instFrame
---------------------------------------------------
tests = TestList test_all
---------------------------------------------------