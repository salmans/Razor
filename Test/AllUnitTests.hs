module Test.AllUnitTests where


import Test.HUnit
import Test.HUnit.Text (runTestTT)

import qualified Test.Chase.Problem.TModel as TModel
import qualified Test.Chase.Problem.TOperations as TOperations
import qualified Test.Chase.Problem.TObservation as TObservation
import qualified Test.Chase.TChase as TChase 

runAllTests = runTestTT $ TestList $
              TModel.test_all ++ TOperations.test_all ++ 
              TObservation.test_all ++ TChase.test_all