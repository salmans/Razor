{- This file is part of Razor.

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

  Module      : SAT.IData
  Description : The module defines the basic datatypes that provide an 
  interface to the SAT-solving modules.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module SAT.IData where

-- Standard
import qualified Data.Map as Map
import Data.List (intercalate)

-- Control
import Control.Applicative
import qualified Control.Monad.State.Lazy as State

-- Common
import Common.Data (SequentLike (..))
import Common.Observation (Observation, ObservationSequent)
import Common.Provenance (Blame)
import Common.Model (Model)

-- Tools
import Tools.Config (Config)


{-| SATAtom is the class of types that can be used as the atomic pieces of 
  information when constructing models using SAT/SMT solving. For instance, in
  a solution based on pure SAT solving (see Picosat.hs) a proposition is the
  atomic data type and in a solution based on SMT solving, an 'SMTObservation'
  is the atomic data type.

  [@emptySATTheory@] returns an empty instance of 'SATTheory' of the type of
  a SATAtom @a@. 
  [@storeSequent@] Given an observational sequent of type 'ObservationSequent',
  and a 'SATTheory' of a 'SATAtom' type @a@, converts the sequent to an instance
  of 'SATSequent' for @a@ and stores it in the theory for future SAT/SMT 
  solving. 
 -}
class (Show a) => SATAtom a where
    emptySATTheory    :: SATTheory a    
    storeSequent      :: SATTheory a -> (Blame, ObservationSequent) -> SATTheory a
    blameSequent      :: SATTheory a -> Blame -> Maybe ObservationSequent

{-| SATSequent is a datatype that represents a sequent in SAT/SMT solving. A 
  type @a@ is the type of atomic formulas in the sequent, e.g., a proposition 
  or an SMT equation. Just like 'ObservationSequent', 'SATSequent' is 
  maintained as two lists representing the body and the head of the sequent:

  [@satSequentBody@] returns the body of a 'SATSequent' as a list of 
  propositions, forming a conjunction.
  [@satSequentHead@] returns the head of a 'SATSequent' as a list of
  propositional conjuncts, forming a DNF. -}
data SATSequent a = SATSequent { satSequentBody :: [a]
                               , satSequentHead :: [[a]] 
                               } deriving (Eq, Ord)

{- Show instance for 'PropSequent' -}
instance (SATAtom a) => Show (SATSequent a) where
    show (SATSequent bdy hd) =
        (show bdy) ++ " -> " ++ showHead ++ "\n"
        where showHead = intercalate " \\/ " (show <$> hd)

{- 'PropSequnent' is a 'SequentLike' instance. -}
instance (SATAtom a) => SequentLike (SATSequent a) where
    toSequent        = undefined
    fromSequent      = undefined
    startSequent     = null.satSequentBody
    failSequent      = null.satSequentHead
    skolemFunctions  = const []
    sequentConstants = const []

{-| SATTheory is a family of types to store a set of 'SATSequent' instances for
  SAT/SMT solving. Different approaches to SAT/SMT solving may need different 
  underlying mechanisms for storing this information, which may be implemented
  as different instances of this type family. -}
data family SATTheory a


instance Show (SATTheory a) where
    show _ = "SATTheory"

{-| SATSolver defines an interface for interacting with a SAT/SMT 
  implementation. Here, @a@ is a 'SATAtom' type, determining the underlying 
  SAT/SMT solution and @b@ is the type of an iterator for maintaining the state
  of SMT computation and fetching next models.
  
  [@satInitialize@] constructs an iterator for fetching models from the current
  state of a 'SATTheory' instance.
  [@satSolve@] given an iterator of type @b@, returns a 'Model' (if exists) and
  a new iterator for fetching the next model.
  [@satClose@] closes the connection to the SMT solver.
  [@satPush@] makes a call to the @push@ function of the underlying SMT solver
  or fakes a push action if the connection to the solver is not incremental.
  [@satPop@] makes a call to the @pop@ function of the underlying SMT solver
  or fakes a pop action if the connection to the solver is not incremental.
-}
class (SATAtom a) => SATSolver a b | b -> a where
    satInitialize :: Config -> SATTheory a -> b
    satSolve      :: b -> (Maybe Model, b)
    satAugment    :: b -> (Maybe Model, b)
    satClose      :: b -> ()
    satPush       :: b -> b
    satPop        :: b -> b