{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
  Razor
  Module      : Chase.Impl
  Description : The module provides the interface to a particular implementation
  of the Chase for constructing the set of possible facts.
  Maintainer  : Salman Saghafi -}

module Chase.Impl ( ChaseSequentType, ChasePossibleFactsType 
                  , chase ) where


-- Control
import Control.Applicative

-- Syntax
import Syntax.GeometricUtils (Theory, preprocess)

-- Common
import Common.Data
import Common.Provenance

-- Chase
import Chase.Data
-- <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
-- Uncomment to select the implementation:

import Chase.PossibleFacts.RelAlg.PossibleFacts
-- <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
import qualified Chase.Chase (chase)

-- Data
import Data.Maybe(fromJust)
-- SAT
import SAT.Impl

-- Tools
import Tools.Config
import Tools.Trace


{-| Gives access to a particular implementation of the chase based on the 
  selected PossibleFacts implementation. -}
chase :: Config -> Theory -> ( ChasePossibleFactsType
                             , ProvInfo
                             , SATTheoryType
                             , Int )
chase cfg thy = let thy'   = preprocess thy
                    seqMap = buildSequentMap $ fromJust <$> fromSequent <$> thy' 
                           :: SequentMap ChaseSequentType
                in  Chase.Chase.chase cfg seqMap


-- TEMPORARY: THIS IS HOW AUGMENTATION WORKS!
-- {-| Gives access to a particular implementation of the chase based on the 
--   selected PossibleFacts implementation. -}
-- chase :: Config -> Theory -> ( ChasePossibleFactsType
--                              , ProvInfo
--                              , SATTheoryType )
-- chase cfg thy = 
--     let thy'   = preprocess thy
--         seqMap = buildSequentMap $ fromSequent <$> thy' 
--                :: SequentMap ChaseSequentType
--         (b, p, t) = Chase.Chase.chase cfg seqMap
--         obs       = Obs $ Rel "R" [Elem $ Element 1]
--         -- obs       = Obs $ FnRel "f" [Elem $ Element 0, Elem $ Element 1]
--         -- obs       = Obs $ Rel "=" [Elem $ Element 0, Elem $ Element 1]

--         -- THESE STEPS SHOULDN'T BE DONE FOR EQUATIONAL FACTS:
--         d        = addToBase obs emptyBase
--         (b', p', t') = Chase.Chase.resumeChase cfg seqMap b d p t
--         ---------------------------------------------------------
--         testSeq   = ObservationSequent [] [[obs]]
--         t''       = storeSequent t' testSeq
--     in  (b', p' ,t'')
