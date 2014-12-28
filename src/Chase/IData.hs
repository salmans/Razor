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

  Module      : Chase.IData
  Description : The module implements the basic data-structures that are used
  by the Chase algorithm.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Chase.IData where

-- Standard
import qualified Data.Map as Map

-- Syntax
import Syntax.Term ( Term (Cons), Constant (..), Variable, Sub, FnSym)
import Syntax.Term ( Term (Elem), Element (..), Variable, Sub, FnSym)
import Syntax.Geometric (Theory, Sequent(..), Formula (..), Atom (..))

-- Control
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import qualified Control.Monad.State.Lazy as State
import qualified Control.Monad.RWS.Lazy as RWS

-- Common
import Common.Basic (Id)
import Common.Data ( SequentLike (..) )
import Common.Provenance (ProvInfo)
import Common.Observation (Observation, ObservationSequent)

--Data
import Data.Maybe(fromJust)

-- SAT
import SAT.Data (SATIterator)

-- Tools
import Tools.Counter (Counter, CounterT)
import Tools.Config (Config, ConfigMonad)

unitName            = "Chase.Data"
error_expectFunAtom = "Expecting a functional Atom!"

{-| PossibleFacts is the class of types that can act as a container for facts
  in some implementation of the Chase, e.g. a database or a term rewrite system.
  
  [@emptyBase@] returns an empty 'PossibleFacts'.
  [@emptyBaseWithConstants@] creates an empty 'PossibleFacts' with a list of
  constants (and their corresponding elements) in them.
  [@nullBase@] returns true if the input 'PossibleFacts' instance is empty.
  [@unionBases@] unions to instances of type 'PossibleFacts'
  [@baseSize@] returns the size of a 'HebrandBase' instance.
  [@addToBase@] adds new information of type 'Observation' to the base. The 
  function is primarily used for augmentation purposes.
 -}
class Show a => PossibleFacts a where
    emptyBase              :: a
    emptyBaseWithConstants :: [Constant] -> a
    nullBase               :: a -> Bool
    unionBases             :: a -> a -> a
    diffBases              :: a -> a -> a
    baseSize               :: a -> Int
    addToBase              :: Observation -> a -> a

{-| 'SequentMap' is a map from 'Id's to instances of a 'SequenceLike' type. -}
type SequentMap s = (SequentLike s) => Map.Map Id s

{-| Creates an instance of 'SequentMap' for a list of 'SequentLike' instances. 
 -}
buildSequentMap :: (SequentLike s) => [s] -> SequentMap s
buildSequentMap seqs =  Map.fromList $ zip [1..] seqs

{-| PullM is the context of computation for retrieving data from a 
  'PossibleFacts'. -}
type PullM h a = (PossibleFacts h) => 
    State.StateT h (State.State ProvInfo) a

{-| Lifting the monad in the 'PullM' stack. -}
liftPullMBase = id

liftPullMProvs :: (Monad m, RWS.MonadTrans t) => m a -> t m a
liftPullMProvs =  liftPullMBase.State.lift

{-| Runs a 'PullM' stack of monads: since the two internal states are not 
  supposed to change in the computation, runPullM does not return their values.
 -}
runPullM :: (PossibleFacts h) => PullM h a -> h -> ProvInfo -> a
runPullM pFn b ps = let runBase = State.evalStateT pFn b
                    in  State.evalState runBase ps

{-| Evaluates a 'Pull' stack of monads. -}
evalPullM :: (PossibleFacts h) => PullM h a -> h -> ProvInfo -> a
evalPullM = runPullM


{-| PushM is the context of computation for building a PossibleFacts. It is a
  Config monad for passing configuration information inside a State monad for
  passing a SAT/SMT iterator, which is used for pushing ground sequents. The 
  combination of the two monads is then wrapped in a Counter monad for keeping
  track of indices of the new elements wrapped in a State transformer to carry
  the Id of sequent being pushed, variables in the body of the sequent, and 
  provenance infromation,. The combination is then wrapped inside another State
  transformer that contains the old database of type 'PossibleFacts', which was
  computed in the previous iteration. -}
type PushM h it a = (PossibleFacts h, SATIterator it) => 
    State.StateT h 
             (State.StateT (Id, [Variable], ProvInfo)
                       (CounterT (State.StateT it ConfigMonad))) a

{-| Lifting the monads in the 'PushM' stack -}
liftPushMBase = id

liftPushMProvs :: (Monad m, RWS.MonadTrans t) => m a -> t m a
liftPushMProvs =  liftPushMBase.State.lift

liftPushMCounter :: ( Monad (t1 m), Monad m
                    , RWS.MonadTrans t, RWS.MonadTrans t1) => m a -> t (t1 m) a
liftPushMCounter = liftPushMProvs.State.lift

liftPushMSATIterator :: ( Monad (t1 (t2 m)), Monad (t2 m)
                        , Monad m, RWS.MonadTrans t
                        , RWS.MonadTrans t1, RWS.MonadTrans t2) 
                        => m a -> t (t1 (t2 m)) a
liftPushMSATIterator =  liftPushMCounter.State.lift

liftPushMConfig :: ( Monad (t1 (t2 (t3 m))), Monad (t2 (t3 m))
                   , Monad (t3 m), Monad m
                   , RWS.MonadTrans t, RWS.MonadTrans t1
                   , RWS.MonadTrans t2, RWS.MonadTrans t3) 
                  => m a -> t (t1 (t2 (t3 m))) a
liftPushMConfig =  liftPushMSATIterator.State.lift

{-| Runs a 'PushM' stack of monads. -}
runPushM :: (PossibleFacts h, SATIterator it)
         => PushM h it a -> h -> (Id, ProvInfo) -> Int -> it -> Config
         -> (a, ProvInfo, Int, it)
runPushM pFn base (id, provs) cnt iter cfg = 
    let runBase = State.evalStateT pFn base
        runProv = State.runStateT runBase (id, [], provs)
        runCntr = State.runStateT runProv cnt
        runThy  = State.runStateT runCntr iter
    in  flatTup $ State.runState runThy cfg
    where flatTup = \((((v, (_, _, w)), x), y), z) -> (v, w, x, y)

{-| ChaseImpl specifies the interface between types that implement a 
  PossibleFacts and its related functions in some implementation of the Chase. 
  The input type parameters are:
    1. [@h@] a container for facts of type 'PossibleFacts'
    2. [@s@] an instance of 'SequentLike', e.g., a pair of relational 
    expressions.
    3. [@r@] is the type of the result set that is pushed from a 'PossibleFacts' 
    @h@ according to an instance @s@ of 'SequentLike'. Intuitively, @r@ contains
    the data in an increment, resulted by evaluating a sequent of type @s@.

  The following functions bind the types @h@, @s@ and @r@:
    1. [@relevant@] Given an instance of @h@, containing the last set of changes
    in the database, decides whether an instance of @s@ is relevant or not. The 
    instance of @s@ is considered to be relevant if the last set of changes may
    have disturbed the balance between the head and the body of the sequent in
    the container.
    2. [@pull@] pulls new information (as a set of observations) from an 
    instance of @h@ as the main container and another instance of @h@ containing
    the last set of changes in the container according to an instance of @s@. 
    The result of this operation is of type @r@.
    3. [@push@] pushes an instance of @r@, resulting from @pull@, to an 
    instance of @h@ (the last argument) for some @s@. This function runs in a 
    'PushM' as the computation context.
    4. [@observationalSequents@] evaluates a sequent of type @s@ in a result set
    of type @r@ and returns all the instances (closed sequents) of type of the
    sequent in the form of 'ObservationSequent's. The function needs an 
    instance of 'ProvInfo' to instantiate existentially quantified variables. It
    also needs two instance of @h@ corresponding to the old and the new bases, 
    to fetch the values for constants in the sequent being converted.
-}
class (PossibleFacts h, SequentLike s, Show r) => 
    ChaseImpl h s r | s h -> r where
    relevant               :: s -> h -> Bool
    pull                   :: s -> h -> h -> PullM h r
    push                   :: (SATIterator it) => s -> r -> h -> PushM h it h
    observationalInstances :: s -> h -> h -> r -> ProvInfo
                           -> [(Sub, ObservationSequent)] 

{-| 'Problem' is a type that passes information from an iteration of the Chase
  to the next iteration. A 'Problem' contains 
  - a map from 'Id' to instances of type 'SequentLike' to keep track of the 
  sequents of the theory in the Chase.
  - an instance of a 'PossibleFacts' type to store the deduced facts.
  - an instance of a 'PossibleFacts' type to store the facts that were deduced in
  the previous iteration as a delta set.
  - an instance of 'ProvInfo' to maintain provenance information for facts and
  elements constructed so far.
  - an instance of 'SATIterator' as a handle for sending propositional sequents
  to the underlying SAT/SMT solver. The propositional sequents are computed in
  parrallel with a run of the Chase that constructs the set of possible facts.
 -}
data Problem h s it where
    Problem :: (PossibleFacts h, SequentLike s, SATIterator it)
               => SequentMap s -> h -> h -> ProvInfo -> it -> Problem h s it

problemSequentMap :: Problem h s it -> SequentMap s
problemSequentMap (Problem m _ _ _ _) = m

problemBase :: Problem h s it -> h
problemBase (Problem _ b _ _ _) = b

problemDelta :: Problem h s it -> h
problemDelta (Problem _ _ d _ _) = d

problemProvs :: Problem h s it -> ProvInfo
problemProvs (Problem _ _ _ p _) = p

problemSATIterator :: Problem h s it -> it
problemSATIterator (Problem _ _ _ _ iter) = iter

{-| Creates an instance of 'Problem'.
  Input:
  - a list of (Id, SequentLike) pairs, providing the sequents of the theory 
  together with their identifiers.
  - an instance of a 'PossibleFacts' type @h@, as the set of deduced facts.
  - another instance of type @h@, as the delta set of facts.
  - an instance of 'ProvInfo' containing the provenance information for 
  elements.
  - an instance of 'SATIterator' for sending propositional sequents to the
  SAT/SMT solver.
 -}
buildProblem :: (ChaseImpl h s r, SATIterator it)
                => [(Id, Sequent)] -> h -> h -> ProvInfo -> it -> Problem h s it
buildProblem seqs db dlt provs iter = 
    let seqMap = Map.fromList $ ((fromJust <$> fromSequent <$>) <$> seqs)
    in  Problem seqMap db dlt provs iter


{-| 'ChaseM' is the computation context for running the Chase. Given a 
  'PossibleFacts' type @h@ and a 'SequentLike' type @s@, the computation context
  is a 'ConfigMonad' in a 'CounterT' wrapped inside an RWS monad transformer 
  with a 'Problem' as its state. The 'CounterT' monad transformer keeps 
  track of the indices for the elements of the model and the ConfigMonad 
  makes user preferences available to the Chase.
 -}
type ChaseM h s a = forall r t it . (ChaseImpl h s r)  => 
    RWS.RWST [String] [String] (Problem h s it)
    (CounterT ConfigMonad) a

liftChaseMState  = id
liftChaseMCounter :: (Monad m, State.MonadTrans t) => m a -> t m a
liftChaseMCounter = liftChaseMState.RWS.lift
liftChaseMConfig  :: ( Monad (t1 m), Monad m
                     , RWS.MonadTrans t, RWS.MonadTrans t1) => m a -> t (t1 m) a
liftChaseMConfig  = liftChaseMCounter.State.lift

{-| Incomplete sequents are those sequents whose heads don't get instantiated 
    because of the maximum Skolem depth for search. This function enforces a 
    contract for such sequents: given the body and the Skolem depth of the 
    quantifier that has reached the maximum depth, the function returns an 
    incomplete sequent. -}
-- FIXME: After developing the idea of "incomplete sequents", the function may
-- be moved to another module.
incompleteSequent :: Formula -> FnSym -> Sequent
incompleteSequent body skFn = Sequent body $ Atm (Inc skFn)


replaceIncomplete :: Atom -> Sequent -> Sequent
replaceIncomplete atm@(FnRel _ _) (Sequent bdy hd) =
  Sequent (incompleteFormula atm bdy) (incompleteFormula atm hd)
replaceIncomplete (Rel _ _) _ = error $ unitName ++ ".replaceIncomplete: " ++
                                        error_expectFunAtom

incompleteFormula :: Atom -> Formula -> Formula
incompleteFormula atom Tru = Tru
incompleteFormula atom Fls = Fls
incompleteFormula atom (Atm atm@(FnRel f _)) =
  if atom == atm then Atm (Inc f) else (Atm atm)
incompleteFormula atom atm@(Atm (Rel _ _)) = atm
incompleteFormula (Rel _ _) _ = error $ unitName ++
                                              ".incompleteFormula: " ++
                                              error_expectFunAtom
incompleteFormula atom (And fmla1 fmla2) =
  let fmla1' = incompleteFormula atom fmla1
      fmla2' = incompleteFormula atom fmla2
  in  case fmla1' of -- shortcut incomplete conjuncts
        Atm (Inc _) -> fmla1'
        otherwise   -> case fmla2' of
                         Atm (Inc _) -> fmla2'
                         otherwise   -> And fmla1' fmla2'
incompleteFormula atom (Or fmla1 fmla2) =
  let fmla1' = incompleteFormula atom fmla1
      fmla2' = incompleteFormula atom fmla2
  in  case fmla1' of
        Atm (Inc _) -> fmla2'
        otherwise   -> case fmla2' of
                         Atm (Inc _) -> fmla1'
                         otherwise   -> Or fmla1' fmla2'
incompleteFormula atom (Exists f x fmla) =
  let fmla' = incompleteFormula atom fmla
  in  case fmla' of
        Atm (Inc _) -> fmla'
        otherwise   -> Exists f x fmla'
incompleteFormula atom (Lone f x fmla unq) =
  let fmla' = incompleteFormula atom fmla
  in  case fmla' of
        Atm (Inc _) -> fmla'
        otherwise   -> Lone f x fmla' unq