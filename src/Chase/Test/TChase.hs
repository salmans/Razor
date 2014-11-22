{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

{- Razor
   Module      : Chase.Test.TChase
   Description : Unit tests for Chase
   Maintainer  : Salman Saghafi -}

module Chase.Test.TChase where

-- Control
import Control.Applicative
import Control.Monad.State.Lazy

-- HUnit
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Syntax
import Syntax.GeometricUtils (parseSequent, preprocess, Element)

-- Common
import Common.Provenance
import Common.Data (PossibleFacts (..), SequentLike (..), SequentMap (..))

-- Chase
import Chase.IChase
import Chase.Data
import Chase.PossibleFacts.RelAlg.PossibleFacts

-- SAT
import SAT.Data

-- Tools
import Tools.Config (Config, defaultConfig)

-- Test
import Chase.Test.TestData

{- Helpers -}
pickTwo (x, y, _) = (x, y) -- (Temporarily)


{- Invoking Relational Algebra implementation of the Chase with default config 
 -}
relationalMap :: [String] -> SequentMap RelSequent
relationalMap thy = let thy'  = parseSequent <$> thy
                    in  sequentMap thy'

relationalChase' :: [String] -> RelPossibleFacts
relationalChase' thy =  chase' defaultConfig (relationalMap thy)

relationalChase :: [String] -> (RelPossibleFacts, ProvInfo, PropTheory)
relationalChase thy =  chase defaultConfig (relationalMap thy)

test_relationalChase' = 
    [ "test relationalChase' for trivial theory" ~:
      emptyBase ~=? relationalChase' thy_triv
    , "test relationalChase' for theory without starting sequents" ~:
      emptyBase ~=? relationalChase' thy_thy101
    , "test relationalChase' for existential element" ~:
      base_rel102 ~=? relationalChase' thy_thy102
    , "test relationalChase' for constants" ~:
      base_rel103 ~=? relationalChase' thy_thy103
    -- handles failure
    , "test relationalChase' for existing constant" ~:
      base_rel104  ~=? relationalChase' thy_thy104


      {- Test theories from the previous implementation -}
    , "test relationalChase' for thy0" ~:
      base_rel0 ~=? relationalChase' thy_thy0
    , "test relationalChase' for thy1" ~:
      base_rel1 ~=? relationalChase' thy_thy1
    , "test relationalChase' for thy2" ~:
      base_rel2 ~=? relationalChase' thy_thy2
    , "test relationalChase' for thy3" ~:
      base_rel3 ~=? relationalChase' thy_thy3
    , "test relationalChase' for thy4" ~:
      base_rel4 ~=? relationalChase' thy_thy4
    , "test relationalChase' for thy5" ~:
      base_rel5 ~=? relationalChase' thy_thy5
    -- , "test relationalChase' for thy6" ~:
    --   base_rel6 ~=? relationalChase' thy_thy6
    -- , "test relationalChase' for thy7" ~:
    --   base_rel7 ~=? relationalChase' thy_thy7
    , "test relationalChase' for thy8" ~:
      base_rel8 ~=? relationalChase' thy_thy8
    , "test relationalChase' for thy9" ~:
      base_rel9 ~=? relationalChase' thy_thy9
    , "test relationalChase' for thy14" ~:
      base_rel14 ~=? relationalChase' thy_thy14
    , "test relationalChase' for thy16" ~:
      base_rel16 ~=? relationalChase' thy_thy16
    , "test relationalChase' for thy17" ~:
      base_rel17 ~=? relationalChase' thy_thy17
    , "test relationalChase' for thy18" ~:
      base_rel18 ~=? relationalChase' thy_thy18

    , "test relationalChase' for thyf0" ~:
      base_relf0 ~=? relationalChase' thy_thyf0
    , "test relationalChase' for thyf1" ~:
      base_relf1 ~=? relationalChase' thy_thyf1
    , "test relationalChase' for thyf2" ~:
      base_relf2 ~=? relationalChase' thy_thyf2
    , "test relationalChase' for thyf3" ~:
      base_relf3 ~=? relationalChase' thy_thyf3
    , "test relationalChase' for thyf4" ~:
      base_relf4 ~=? relationalChase' thy_thyf4
    , "test relationalChase' for thyf5" ~:
      base_relf5 ~=? relationalChase' thy_thyf5
    , "test relationalChase' for thyf6" ~:
      base_relf6 ~=? relationalChase' thy_thyf6
    , "test relationalChase' for thyf8" ~:
      emptyBase ~=? relationalChase' thy_thyf8
    , "test relationalChase' for thyf9" ~:
      base_relf9 ~=? relationalChase' thy_thyf9
    , "test relationalChase' for thyf10" ~: -- fix the orfering if possible
      base_relf10 ~=? relationalChase' thy_thyf10
    , "test relationalChase' for thyf12" ~: 
      base_relf12 ~=? relationalChase' thy_thyf12
    ]

test_relationalChase = 
    [ "test relationalChase for trivial theory" ~:
      (emptyBase, emptyProvInfo, propThy_triv) ~=? 
      relationalChase thy_triv
    , "test relationalChase for theory without starting sequents" ~:
      (emptyBase, emptyProvInfo, propThy_thy101) ~=? 
      relationalChase thy_thy101
    , "test relationalChase for existential element" ~:
      (base_rel102, prov_rel102, propThy_thy102) ~=? 
      relationalChase thy_thy102
    , "test relationalChase for constants" ~:
      (base_rel103, prov_rel103, propThy_thy103) ~=? 
      relationalChase thy_thy103
    -- handles failure
    , "test relationalChase for existing constant" ~:
      (base_rel104, prov_rel104, propThy_thy104) ~=? 
      relationalChase thy_thy104


      {- Test theories from the previous implementation -}
    , "test relationalChase for thy0" ~:
      (base_rel0, prov_rel0, propThy_thy0) ~=? 
      relationalChase thy_thy0
    , "test relationalChase for thy1" ~:
      (base_rel1, prov_rel1, propThy_thy1) ~=? 
      relationalChase thy_thy1
    , "test relationalChase for thy2" ~:
      (base_rel2, prov_rel2, propThy_thy2) ~=? 
      relationalChase thy_thy2
    , "test relationalChase for thy3" ~:
      (base_rel3, prov_rel3, propThy_thy3) ~=? 
      relationalChase thy_thy3
    , "test relationalChase for thy4" ~:
      (base_rel4, prov_rel4, propThy_thy4) ~=? 
      relationalChase thy_thy4
    , "test relationalChase for thy5" ~:
      (base_rel5, prov_rel5, propThy_thy5) ~=? 
      relationalChase thy_thy5
    -- , "test relationalChase for thy6" ~:
    --   (base_rel6, prov_rel6, propThy_thy6) ~=? 
    --   relationalChase thy_thy6
    -- , "test relationalChase for thy7" ~:
    --   (base_rel7, prov_rel7, propThy_thy7) ~=? 
    --   relationalChase thy_thy7
    , "test relationalChase for thy8" ~:
      (base_rel8, emptyProvInfo) ~=? pickTwo (relationalChase thy_thy8)
    , "test relationalChase for thy9" ~:
      (base_rel9, prov_rel9) ~=? pickTwo (relationalChase thy_thy9)
    , "test relationalChase for thy14" ~:
      (base_rel14, prov_rel14) ~=? pickTwo (relationalChase thy_thy14)
    , "test relationalChase for thy16" ~:
      (base_rel16, prov_rel16) ~=? pickTwo (relationalChase thy_thy16)
    , "test relationalChase for thy17" ~:
      (base_rel17, prov_rel17) ~=? pickTwo (relationalChase thy_thy17)
    , "test relationalChase for thy18" ~:
      (base_rel18, prov_rel18) ~=? pickTwo (relationalChase thy_thy18)

    , "test relationalChase for thyf0" ~:
      (base_relf0, prov_relf0) ~=? pickTwo (relationalChase thy_thyf0)
    , "test relationalChase for thyf1" ~:
      (base_relf1, prov_relf1) ~=? pickTwo (relationalChase thy_thyf1)
    , "test relationalChase for thyf2" ~:
      (base_relf2, prov_relf2) ~=? pickTwo (relationalChase thy_thyf2)
    , "test relationalChase for thyf3" ~:
      (base_relf3, prov_relf3) ~=? pickTwo (relationalChase thy_thyf3)
    , "test relationalChase for thyf4" ~:
      (base_relf4, prov_relf4) ~=? pickTwo (relationalChase thy_thyf4)
    , "test relationalChase for thyf5" ~:
      (base_relf5, prov_relf5) ~=? pickTwo (relationalChase thy_thyf5)
    , "test relationalChase for thyf6" ~:
      (base_relf6, prov_relf6) ~=? pickTwo (relationalChase thy_thyf6)
    , "test relationalChase for thyf8" ~:
      (emptyBase, emptyProvInfo) ~=? pickTwo (relationalChase thy_thyf8)
    , "test relationalChase for thyf9" ~:
      (base_relf9, prov_relf9) ~=? pickTwo (relationalChase thy_thyf9)
    , "test relationalChase for thyf10" ~: -- fix the orfering if possible
      (base_relf10, prov_relf10) ~=? pickTwo (relationalChase thy_thyf10)
    , "test relationalChase for thyf12" ~: 
      (base_relf12, prov_relf12) ~=? pickTwo (relationalChase thy_thyf12)
    ]

test_all = test_relationalChase' ++ test_relationalChase