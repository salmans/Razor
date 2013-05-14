module Test.Chase.Problem.TModel where


import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- unit under test:
import Formula.SyntaxGeo

import Chase.Problem.IModel

import Test.TestData
import Test.TestHelper

import Chase.Problem.Model (Model (..))
import qualified Chase.Problem.Model as Model
import Chase.Problem.Structures (ID(..), 
                                 Frame(Frame), 
                                 Problem(Problem))
import qualified CC.CC as CC
import Debug.Trace

import Data.Set as Set
---------------------------------------------------
test_add = 
    ["test empty" ~: 
     True ~=? (equalModels (add Model.empty []) Model.empty),
     "test add nothing" ~:
     True ~=? (equalModels (add mdl1 []) mdl1),
     "test fact and empty model" ~: 
     True ~=? (equalModels (add Model.empty [obs5]) mdl1),
     "test equality and empty model" ~:
     True ~=? (equalModels (add Model.empty [obs7]) mdl2),
     "test combined and empty model" ~:
     True ~=? (equalModels (add Model.empty [obs5, obs6, obs7]) mdl3),
     "test fact idempotency" ~:
     True ~=? (equalModels (add mdl1 [obs5]) mdl1),
     "test equality idempotency" ~:
     True ~=? (equalModels (add mdl2 [obs7]) mdl2),
     "test connected equations" ~:
     True ~=? (equalModels (add mdl3 [obs16]) mdl4)]
---------------------------------------------------
test_obsToEquation = 
    ["test Fact" ~: cceq1 ~=? (obsToEquation obs5),
     "test Equality" ~: cceq2 ~=? (obsToEquation obs7)]
---------------------------------------------------
test_isTrue = 
    ["test equality is true" ~: True ~=? (isTrue mdl2 obs7),
     "test equality is not true" ~: False ~=? (isTrue mdl1 obs7),
     "test fact is true" ~: True ~=? (isTrue mdl3 obs5),
     "test fact is not true" ~: False ~=? (isTrue mdl1 obs6),
                                 -- Truth exists in every model:
     "test truth exists 1" ~: True ~=? (isTrue Model.empty obs0),
     "test truth exists 2" ~: True ~=? (isTrue mdl1 obs0),
     "test truth exists 3" ~: True ~=? (isTrue mdl2 obs0)]
---------------------------------------------------
test_areEqual = 
    ["test equal terms" ~: True ~=? (areEqual mdl2 trm8 trm9),
     "test non-equal terms" ~: False ~=? (areEqual mdl1 trm8 truth),
     "test non-existing terms" ~: False ~=? (areEqual mdl1 trm8 trm9),
     "test against a non-existing term" ~: 
     False ~=? (areEqual mdl2 trm8 trm18)]
    ---------------------------------------------------
test_denotes = 
    ["test truth" ~: obs0 ~=? (denotes mdl1 obs0),
     "test true fact" ~: obs0 ~=? (denotes mdl1 obs5),
     "test non-true fact" ~: obs6 ~=? (denotes mdl1 obs6)]
---------------------------------------------------
test_all = test_add ++ test_obsToEquation ++ test_isTrue ++ 
           test_areEqual ++ test_denotes
---------------------------------------------------
tests = TestList test_all
---------------------------------------------------