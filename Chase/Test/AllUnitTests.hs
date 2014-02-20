module Chase.Test.AllUnitTests where


import Chase.Test.HUnit
import Chase.Test.HUnit.Text (runTestTT)

import qualified Chase.Test.Chase.Problem.TModel as TModel
import qualified Chase.Test.Chase.Problem.TOperations as TOperations
import qualified Chase.Test.Chase.Problem.TObservation as TObservation
import qualified Chase.Test.Chase.TChase as TChase 

runAllTests = runTestTT $ TestList $
              TModel.test_all ++ TOperations.test_all ++ 
              TObservation.test_all ++ TChase.test_all