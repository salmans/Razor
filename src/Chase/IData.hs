{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}

{- Razor
   Module      : Chase.IData
   Description : The module implements the basic data-structures that are used
   by the Chase algorithm.
   Maintainer  : Salman Saghafi -}

module Chase.IData where

-- Standard
import qualified Data.Map as Map

-- Syntax
import Syntax.Term (Constant)
import Syntax.Geometric (Theory, Sequent(..), Formula (..))

-- Control
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import qualified Control.Monad.State.Lazy as State
import qualified Control.Monad.RWS.Lazy as RWS

-- Common
import Common.Basic (Id)
import Common.Data ( SequentLike (..), ConstantValueMap )
import Common.Provenance (ProvInfo)
import Common.Observation (ObservationSequent)

-- SAT
import SAT.Data (SATAtom, SATTheory)

-- Tools
import Tools.Counter (Counter, CounterT)
import Tools.Config (Config, ConfigMonad)


{-| HerbrandBase is the class of types that can act as a container for facts
  in some implementation of the Chase, e.g. a database or a term rewrite system.
  
  [@emptyBase@] returns an empty 'HerbrandBase'.
  [@emptyBaseWithConstants@] creates an empty 'HerbrandBase' with a list of
  constants (and their corresponding elements) in them.
  [@nullBase@] returns true if the input 'HerbrandBase' instance is empty.
  [@unionBases@] unions to instances of type 'HerbrandBase'
  [@baseSize@] returns the size of a 'HebrandBase' instance.
 -}
class Show a => HerbrandBase a where
    emptyBase              :: a
    emptyBaseWithConstants :: [Constant] -> a
    nullBase               :: a -> Bool
    unionBases             :: a -> a -> a
    diffBases              :: a -> a -> a
    baseSize               :: a -> Int
    baseConstants          :: a -> ConstantValueMap

{-| 'SequentMap' is a map from 'Id's to instances of a 'SequenceLike' type. -}
type SequentMap s = (SequentLike s) => Map.Map Id s

{-| Creates an instance of 'SequentMap' for a list of 'SequentLike' instances. 
 -}
buildSequentMap :: (SequentLike s) => [s] -> SequentMap s
buildSequentMap seqs =  Map.fromList $ zip [1..] seqs

{-| PullM is the context of computation for retrieving data from a 
  'HerbrandBase'. -}
type PullM h a = (HerbrandBase h) => 
    State.StateT h (State.State ProvInfo) a

{-| Lifting the monad in the 'PullM' stack. -}
liftPullMBase = id

liftPullMProvs :: (Monad m, RWS.MonadTrans t) => m a -> t m a
liftPullMProvs =  liftPullMBase.State.lift

{-| Runs a 'PullM' stack of monads: since the two internal states are not 
  supposed to change in the computation, runPullM does not return their values.
 -}
runPullM :: (HerbrandBase h) => PullM h a -> h -> ProvInfo -> a
runPullM pFn b ps = let runBase = State.evalStateT pFn b
                    in  State.evalState runBase ps

{-| Evaluates a 'Pull' stack of monads. -}
evalPullM :: (HerbrandBase h) => PullM h a -> h -> ProvInfo -> a
evalPullM = runPullM


{-| PushM is the context of computation for building a HerbrandBase. It is a
  Counter monad for keeping track of indices of the new elements wrapped in
  a State transformer to carry provenance infromation. The combination is then 
  wrapped inside another State transformer that contains the old database of
  type 'HerbrandBase', which was computed in the previous iteration. -}
type PushM h t a = (HerbrandBase h, SATAtom t) => 
    State.StateT h 
             (State.StateT ProvInfo 
                       (CounterT (State.StateT (SATTheory t) ConfigMonad))) a

{-| Lifting the monads in the 'PushM' stack -}
liftPushMBase = id

liftPushMProvs :: (Monad m, RWS.MonadTrans t) => m a -> t m a
liftPushMProvs =  liftPushMBase.State.lift

liftPushMCounter :: ( Monad (t1 m), Monad m
                    , RWS.MonadTrans t, RWS.MonadTrans t1) => m a -> t (t1 m) a
liftPushMCounter = liftPushMProvs.State.lift

liftPushMSATTheory :: ( Monad (t1 (t2 m)), Monad (t2 m)
                      , Monad m, RWS.MonadTrans t
                      , RWS.MonadTrans t1, RWS.MonadTrans t2) 
                      => m a -> t (t1 (t2 m)) a
liftPushMSATTheory =  liftPushMCounter.State.lift

liftPushMConfig :: ( Monad (t1 (t2 (t3 m))), Monad (t2 (t3 m))
                   , Monad (t3 m), Monad m
                   , RWS.MonadTrans t, RWS.MonadTrans t1
                   , RWS.MonadTrans t2, RWS.MonadTrans t3) 
                  => m a -> t (t1 (t2 (t3 m))) a
liftPushMConfig =  liftPushMSATTheory.State.lift

{-| Runs a 'PushM' stack of monads. -}
runPushM :: (HerbrandBase h, SATAtom t) => PushM h t a -> h -> ProvInfo
         -> Int -> SATTheory t -> Config -> (a, ProvInfo, Int, SATTheory t)
runPushM pFn base provs cnt propThy cfg = 
    let runBase = State.evalStateT pFn base
        runProv = State.runStateT runBase provs
        runCntr = State.runStateT runProv cnt
        runThy  = State.runStateT runCntr propThy
    in  flatTup $ State.runState runThy cfg
    where flatTup = \((((v, w), x), y), z) -> (v, w, x, y)

{-| Evaluates a 'PushM' stack of monads and returns the resulting 
  'HerbrandBase'-}
evalPushM :: (HerbrandBase h, SATAtom t) => PushM h t a -> h -> ProvInfo 
          -> Int -> SATTheory t -> Config -> a
evalPushM pFn base provs cnt propThy cfg = 
    let (x, _, _, _) = runPushM pFn base provs cnt propThy cfg in x

{-| HerbrandImpl specifies the interface between types that implement a 
  HerbrandBase and its related functions in some implementation of the Chase. 
  The input type parameters are:
    1. [@h@] a container for facts of type 'HerbrandBase'
    2. [@s@] an instance of 'SequentLike', e.g., a pair of relational 
    expressions.
    3. [@r@] is the type of the result set that is pushed from a 'HerbrandBase' 
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
class (HerbrandBase h, SequentLike s, Show r) => 
    HerbrandImpl h s r | s h -> r where
    relevant               :: s -> h -> Bool
    pull                   :: s -> h -> h -> PullM h r
    push                   :: (SATAtom t) => s -> r -> h -> PushM h t h
    observationalInstances :: s -> h -> h -> r -> ProvInfo
                           -> [ObservationSequent]

{-| 'Problem' is a type that passes information from an iteration of the Chase
  to the next iteration. A 'Problem' contains 
  - a map from 'Id' to instances of type 'SequentLike' to keep track of the 
  sequents of the theory in the Chase.
  - an instance of a 'HerbrandBase' type to store the deduced facts.
  - an instance of a 'HerbrandBase' type to store the facts that were deduced in
  the previous iteration as a delta set.
  - an instance of 'ProvInfo' to maintain provenance information for facts and
  elements constructed so far.
  - an instance of 'PropTheory', containing propositional instances of the 
  first-order theory, which is computed in parallel with the run of the Chase.
 -}
data Problem h s t where
    Problem :: (HerbrandBase h, SequentLike s, SATAtom t) 
               => SequentMap s -> h -> h -> ProvInfo 
                               -> SATTheory t -> Problem h s t

problemSequentMap :: Problem h s t -> SequentMap s
problemSequentMap (Problem m _ _ _ _) = m

problemBase :: Problem h s t -> h
problemBase (Problem _ b _ _ _) = b

problemDelta :: Problem h s t -> h
problemDelta (Problem _ _ d _ _) = d

problemProvs :: Problem h s t -> ProvInfo
problemProvs (Problem _ _ _ p _) = p

problemSATTheory :: Problem h s t -> SATTheory t
problemSATTheory (Problem _ _ _ _ p) = p

{-| Creates an instance of 'Problem'.
  Input:
  - a list of (Id, SequentLike) pairs, providing the sequents of the theory 
  together with their identifiers.
  - an instance of a 'HerbrandBase' type @h@, as the set of deduced facts.
  - another instance of type @h@, as the delta set of facts.
  - an instance of 'ProvInfo' containing the provenance information for 
  elements.
  - an instance of 'PropTheory' containing propositional instances of the input
  theory in the input 'HerbrandBase' set.
 -}
buildProblem :: (HerbrandImpl h s r, SATAtom t) 
                => [(Id, Sequent)] -> h -> h -> ProvInfo 
                                   -> SATTheory t -> Problem h s t
buildProblem seqs db dlt provs propTheory = 
    let seqMap = Map.fromList $ ((fromSequent <$>) <$> seqs)
    in  Problem seqMap db dlt provs propTheory


{-| 'ChaseM' is the computation context for running the Chase. Given a 
  'HerbrandBase' type @h@ and a 'SequentLike' type @s@, the computation context
  is a 'ConfigMonad' in a 'CounterT' wrapped inside an RWS monad transformer 
  with a 'Problem' as its state. The 'CounterT' monad transformer keeps 
  track of the indices for the elements of the model and the ConfigMonad 
  makes user preferences available to the Chase.
 -}
type ChaseM h s a = forall r t . (HerbrandImpl h s r)  => 
    RWS.RWST [String] [String] (Problem h s t) 
    (CounterT ConfigMonad) a

liftChaseMState  = id
liftChaseMCounter :: (Monad m, State.MonadTrans t) => m a -> t m a
liftChaseMCounter = liftChaseMState.RWS.lift
liftChaseMConfig  :: ( Monad (t1 m), Monad m
                     , RWS.MonadTrans t, RWS.MonadTrans t1) => m a -> t (t1 m) a
liftChaseMConfig  = liftChaseMCounter.State.lift