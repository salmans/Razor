module Test.AllUnitTests where


import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Common
import qualified Common.Test.TBasic as TBasic
import qualified Common.Test.TObservation as TObservation
import qualified Common.Test.TProvenance as TProvenance

-- Syntax
import qualified Syntax.Test.TTerm as TTerm
import qualified Syntax.Test.TGeometric as TGeometric
import qualified Syntax.Test.TFirstOrder as TFirstOrder
import qualified Syntax.Test.TGeometricUtils as TGeometricUtils
import qualified Syntax.Test.TFirstOrderUtils as TFirstOrderUtils

-- RelAlg
import qualified Chase.HerbrandBase.RelAlg.Test.TDB as TDB
import qualified Chase.HerbrandBase.RelAlg.Test.TLang as TLang
import qualified Chase.HerbrandBase.RelAlg.Test.TTranslate as TTranslate

-- Chase
import qualified Chase.Test.TChase as TChase

-- Tools
import qualified Tools.Test.TUtils as TUtils
import qualified Tools.Test.TCounter as TCounter


runAllTests = runTestTT $ TestList $ 
              -- Common
              TBasic.test_all
              ++ TObservation.test_all
              ++ TProvenance.test_all

              -- Tools
              ++ TUtils.test_all ++ TCounter.test_all 

              -- Syntax
              ++ TTerm.test_all 
              ++ TGeometric.test_all ++ TFirstOrder.test_all
              ++ TGeometricUtils.test_all ++ TFirstOrderUtils.test_all

              -- Chase.HerbrandBase.RelAlg
              ++ TTranslate.test_all ++ TLang.test_all ++ TDB.test_all

              -- Chase
              ++ TChase.test_all

