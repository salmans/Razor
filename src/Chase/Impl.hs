{-| This file is part of Razor.

  Razor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Razor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Razor.  If not, see <http://www.gnu.org/licenses/>.

  Module      : Chase.Impl
  Description : The module provides the interface to a particular implementation
  of the Chase for constructing the set of possible facts.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Chase.Impl ( ChaseSequentType, ChasePossibleFactsType 
                  , chase, resume) where


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
import qualified Chase.Chase (chase, resumeChase)

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
                             , SATIteratorType
                             , Int )
chase cfg thy = let thy'   = preprocess thy
                    seqMap = buildSequentMap $ fromJust <$> fromSequent <$> thy' 
                           :: SequentMap ChaseSequentType
                    (b, p, it, c) = Chase.Chase.chase cfg seqMap
                in  (b, p, it, c)

resume :: Config -> Int -> SequentMap ChaseSequentType -> ChasePossibleFactsType
       -> ChasePossibleFactsType -> ProvInfo -> SATIteratorType
       -> (ChasePossibleFactsType, ProvInfo, SATIteratorType, Int)
resume = Chase.Chase.resumeChase
