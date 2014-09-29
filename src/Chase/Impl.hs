{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
  Razor
  Module      : Chase.Impl
  Description : The module provides the interface to a particular implementation
  of the Chase for constructing the set of possible facts.
  Maintainer  : Salman Saghafi -}

module Chase.Impl ( ChaseSequentType, ChaseHerbrandBaseType 
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

import Chase.HerbrandBase.RelAlg.HerbrandBase
-- <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
import qualified Chase.Chase (chase)

-- SAT
import SAT.Impl

-- Tools
import Tools.Config
import Tools.Trace


{-| Gives access to a particular implementation of the chase based on the 
  selected HerbrandBase implementation. -}
chase :: Config -> Theory -> ( ChaseHerbrandBaseType
                             , ProvInfo
                             , SATTheoryType )
chase cfg thy = let thy'   = preprocess thy
                    seqMap = buildSequentMap $ fromSequent <$> thy' 
                           :: SequentMap ChaseSequentType
                in  Chase.Chase.chase cfg seqMap