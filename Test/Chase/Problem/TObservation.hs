module Test.Chase.Problem.TObservation where


import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- unit under test:
import Formula.SyntaxGeo

import Chase.Problem.Observation
import Chase.Problem.Model(truth)

import Test.TestData
import Test.TestHelper

import qualified CC.CC as CC
import Debug.Trace

import Data.Set as Set
import Data.Map as Map
---------------------------------------------------
test_obsFuncSyms = 
    ["test truth" ~: 
     True ~=? (equalLists (obsFuncSyms obs0) ["True"]),
     "test no constants" ~: 
     True ~=? (equalLists (obsFuncSyms obs1) ["P"]),
     "test constants" ~: 
     True ~=? (equalLists (obsFuncSyms obs5) ["P", "a"]),
     "test functions" ~: 
     True ~=? (equalLists (obsFuncSyms obs7) ["=", "f", "a"])]
---------------------------------------------------
test_obsSym = 
    ["test truth" ~: 
     True ~=? (equalLists (obsSym obs0) "True"),
     "test no constants" ~: 
     True ~=? (equalLists (obsSym obs1) "P"),
     "test constants" ~: 
     True ~=? (equalLists (obsSym obs5) "P"),
     "test functions" ~: 
     True ~=? (equalLists (obsSym obs7) "=")]
---------------------------------------------------
test_obsSubterms = 
    ["test truth" ~: 
     True ~=? (equalLists (obsSubterms obs0) [obs0]),
     "test fact" ~: 
     True ~=? (equalLists (obsSubterms obs5) [obs5, obs19]),
     "test equality" ~: 
     True ~=? (equalLists (obsSubterms obs7) [obs7, obs19, obs20]),
     "test complex observation" ~: 
     True ~=? (equalLists (obsSubterms obs18) 
               [obs18, obs19, obs20, obs21])]
---------------------------------------------------
test_replaceObsSubterms = 
    ["test no occurrence" ~: 
     obs20 ~=? (replaceObsSubterms obs17 obs21 obs20),
     "test replace in fact" ~:
     obs5 ~=? (replaceObsSubterms obs20 obs19 obs22),
     "test replace in equality" ~:
     obs7 ~=? (replaceObsSubterms obs23 obs19 obs16),
     "test replace in denotation" ~:
     obs23 ~=? (replaceObsSubterms obs20 obs19 obs21),
     "test replace entire observation" ~:
     obs20 ~=? (replaceObsSubterms obs21 obs20 obs21)]
---------------------------------------------------
test_matchObs = 
    ["test closed observations" ~: 
     Nothing ~=? (matchObs obs5 obs6),
     "test no match" ~: 
     Nothing ~=? (matchObs obs9 obs5),
     "test equal observations" ~:
     Just sub0 ~=? (matchObs obs5 obs5),
     "test no match (reversed positions)" ~: 
     Nothing ~=? (matchObs obs6 obs9),
     "test match on variable" ~: 
     Just sub1  ~=? (matchObs obs12 obs13),
     "test match on constant" ~: 
     Just sub2  ~=? (matchObs obs8 obs22),
     "test match on term with function" ~: 
     Just sub3  ~=? (matchObs obs1 obs22),
     "test match inside functions" ~: 
     Just sub4  ~=? (matchObs obs3 obs18)]
---------------------------------------------------
test_matchObsSubterms = 
    ["test no match" ~: 
     [] ~=? (matchObsSubterms obs5 obs6),
     "test equal observations" ~: 
     [sub0] ~=? (matchObsSubterms obs21 obs21),
     "test closed observations" ~: 
     [sub0, sub0] ~=? (matchObsSubterms obs19 obs7),
     "test match variable" ~: 
     [sub3] ~=? (matchObsSubterms obs20 obs12),
     -- Even though it seems like this function has to find two 
     -- substitutions x -> a and x -> f(a) for f(a) and f(x), it
     -- looks like we can survive with only x -> a. This is what
     -- matchObsSubterms does now: 
     "test multiple matches 1" ~: 
     [sub2] ~=? (matchObsSubterms obs20 obs8),
     "test multiple matches 2" ~: 
     [sub2, sub5] ~=? (matchObsSubterms obs20 obs4)]
---------------------------------------------------
test_obsToTerm = 
    ["test truth" ~: truth ~=? (obsToTerm obs0),
     "test fact" ~: trm1 ~=? (obsToTerm obs1),
     "test equality" ~: (Fn "=" [trm4, trm5]) ~=? (obsToTerm obs4),
     "test denotation" ~: trm23 ~=? (obsToTerm obs21)]
---------------------------------------------------
test_termToObs = 
    ["test truth" ~: obs0 ~=? (termToObs False truth),
     "test fact" ~: obs22 ~=? (termToObs True trm24),
     "test equality with True" ~: 
     obs16 ~=? (termToObs True (Fn "=" [trm8, trm18])),
     "test equality with False" ~: 
     obs16 ~=? (termToObs True (Fn "=" [trm8, trm18]))]
---------------------------------------------------
test_atomToTerm = 
    ["test 1" ~: trm1 ~=? (atomToTerm atm1),
     "Test 2" ~: trm6 ~=? (atomToTerm atm6),
     "test 3" ~: trm24 ~=? (atomToTerm atm24)]
---------------------------------------------------
test_termToAtom = 
    ["test 1" ~: atm1 ~=? (termToAtom trm1),
     "test 2" ~: atm6 ~=? (termToAtom trm6),
     "test 3" ~: atm24 ~=? (termToAtom trm24)]
---------------------------------------------------
test_all = test_obsFuncSyms ++ test_obsSym ++ test_obsSubterms ++ 
           test_replaceObsSubterms ++ test_matchObs ++ 
           test_matchObsSubterms ++ test_obsToTerm ++ 
           test_termToObs ++ test_atomToTerm ++ test_termToAtom
---------------------------------------------------
tests = TestList test_all
---------------------------------------------------

