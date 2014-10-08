{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DoAndIfThenElse #-}

{- Razor
   Module      : Chase.IChase
   Description : This module contains the internal implementation of the Chase.
   Maintainer  : Salman Saghafi -}
module Chase.IChase where

-- Standard
import qualified Data.Map as Map
import Data.Maybe

-- Control
import Control.Monad
import Control.Applicative
import qualified Control.Monad.RWS.Lazy as RWS
import qualified Control.Monad.State.Lazy as State

-- Syntax
import Syntax.GeometricUtils (Theory, preprocess, Constant (..))

-- Common
import Common.Basic (Id)
import Common.Data ( SequentLike (..) )
import Common.Provenance

-- Chase
import Chase.Data

-- SAT
import SAT.Data

-- Tools
import Tools.Config (Config (..), defaultConfig)
import Tools.FolToGeo (parseFolToSequents)


{-| Runs the Chase algorithm and returns the resulting base and the resulting 
  provenance information.
  Input: 
  - A 'Config' instance, providing user preferences. 
  - An instance of 'SequentMap' for a 'SequentLike' type @s@. 

  Output:
  A pair of a base of type @h@ and its corresponding provenance information of
  type 'ProvInfo'. -}

chase :: (HerbrandImpl h s r, SATAtom t) => Config -> SequentMap s 
      -> (h, ProvInfo, SATTheory t)
chase cfg seqs = let (b, p, pt, _) = runChase cfg seqs Nothing Nothing Nothing
                 in  (b, p, pt)

{-| Runs the Chase algorithm and returns the resulting base. -}
chase' :: (HerbrandImpl h s r) => Config -> SequentMap s -> h
chase' cfg seqs = undefined -- evalChase cfg seqs Nothing Nothing Nothing

{-| Runs the Chase algorithm, ensuring that the initial set of constants will
  denote elements in the output instnace of 'HerbrandBase'. -}
chaseWithInitialConstants :: (HerbrandImpl h s r, SATAtom t) => Config 
                          -> SequentMap s -> [Constant] 
                          -> (h, ProvInfo, SATTheory t)
chaseWithInitialConstants cfg seqs consts = 
    let initialBase   = emptyBaseWithConstants consts
        initialProvs  = emptyProvInfoWithConstants consts
        (b, p, pt, _) = runChase cfg seqs (Just initialBase) (Just initialProvs)
                        Nothing
    in  (b, p, pt)

{-| Runs the Chase algorithm, ensuring that the initial set of constants will
  denote elements in the output instnace of 'HerbrandBase'. -}
chaseWithInitialConstants' :: HerbrandImpl h s r => Config -> SequentMap s 
                           -> [Constant] -> h
chaseWithInitialConstants' cfg seqs consts = undefined
    -- let initialBase  = emptyBaseWithConstants consts
    --     initialProvs = emptyProvInfoWithConstants consts
    -- in  evalChase cfg seqs (Just initialBase) (Just initialProvs) Nothing

{-| Runs the (monadic) Chase for instances of type @h@ and @s@ and @r@ that 
  implement a 'HerbrandImpl'.
  Input:
  - An instance of 'Config' specifying user preferences.
  - An 'SequentMap' of a 'SequentLike' type, @s@, containing the sequents of the
  input theory.
  - Maybe an instance of a 'HerbrandBase' type, @h@, as the initial base.
  - Maybe an instance of 'ProvInfo', as the provenance information for the 
  initial base.
  
  Output:
  A tuple containing the following:
  - An instance of type @h@, containing all the deduced facts based on the 
  initial instances and the initial configuration.
  - An instance of 'ProvInfo' that contains the provenance information for the
  facts and the elements of the output base.
  - Log information.
-}
runChase :: (HerbrandImpl h s r, SATAtom t) => 
            Config -> SequentMap s -> Maybe h -> Maybe ProvInfo 
         -> Maybe (SATTheory t) -> (h, ProvInfo, SATTheory t, [String])
runChase cfg seqs initBase initProvs initPropThy = 
    let runCM       = RWS.runRWST chaseM []
                      (Problem seqs emptyBase (fromMaybe emptyBase initBase)
                       (fromMaybe emptyProvInfo initProvs)
                       (fromMaybe emptySATTheory initPropThy))
        runCt       = State.evalStateT runCM 0
        (base, prob, log) 
                    = State.evalState runCt cfg
    in (base, problemProvs prob, problemSATTheory prob, log)
    where seqs' = Map.filter (not.failSequent) seqs
                  -- filter out failing sequents- don't process them

{-| Just like 'runChase' runs the (monadic) Chase for instances of type @h@ 
  and @s@ and @r@ that implement a 'HerbrandImpl' but only returns the resulting
  'HerbrandBase' instance.
  Input:
  - An instance of 'Config' specifying user preferences.
  - An 'SequentMap' of a 'SequentLike' type, @s@, containing the sequents of the
  input theory.
  - Maybe an instance of a 'HerbrandBase' type, @h@, as the initial base.
  - Maybe an instance of 'ProvInfo', as the provenance information for the 
  initial base.
  
  Output:
  An instance of type @h@, containing all the deduced facts based on the initial
  instances and the initial configuration.
-}
evalChase :: (HerbrandImpl h s r, SATAtom t) => 
            Config -> SequentMap s -> Maybe h -> Maybe ProvInfo 
          -> Maybe (SATTheory t) -> h
evalChase cfg seqs initBase initProvs propThy = 
    let (b, _, _, _) = runChase cfg seqs initBase initProvs propThy
    in  b

{-| Runs the Chase in the context of 'ChaseM' for some realization of the 
  Chase, implemented as 'HerbrandImpl'. -}
chaseM :: HerbrandImpl h s r => ChaseM h s h
chaseM = do
  cfg <- liftChaseMConfig State.get
  Problem seqs base dlt provs propThy <- liftChaseMState RWS.get
  let (startSeqs, restSeqs) = Map.partition startSequent seqs
                              -- Separate sequents with empty body
  counter <- (liftChaseMCounter State.get)
  let (newDlt, newProvs, counter', propThy') = 
          iterateBalance startSeqs base dlt provs counter propThy cfg
                           -- Balance the sequents with empty body right here
                           -- before iterating through the remaining sequents

  liftChaseMCounter (State.put counter') -- Update the new counter
  liftChaseMState $ RWS.put $ Problem restSeqs (unionBases base dlt)
                  newDlt newProvs propThy'
     -- Update the computation state with new Herbrand base and delta base. 
     -- Also, throw away the starting sequents
  stepsM -- Iterate through remaining sequents

{- As a helper for 'chase', iterates through the sequents of the theory (with 
   non-empty bodies) recursively until the changes in one iteration is contained
   by the existing base. 
 -}
stepsM :: HerbrandImpl h s r => ChaseM h s h
stepsM = do
  cfg <- liftChaseMConfig State.get
  Problem seqs base dlt provs propThy <- liftChaseMState (RWS.get)
  if nullBase dlt -- If the current delta contains nothing new
  then return base -- return the current base
  else do          -- else make a new step
    let seqs' = Map.filter ((flip relevant) dlt) seqs
                -- Select sequents that are relevant to the last set of changes
    counter <- (liftChaseMCounter State.get)
    let (newDlt, newProvs, counter', propThy') = 
            iterateBalance seqs' base dlt provs counter propThy cfg
        -- Process the relevant sequents
    liftChaseMCounter (State.put counter')
    liftChaseMState $ RWS.put $ Problem seqs (unionBases base dlt) 
                    newDlt newProvs propThy'
       -- Update the state and prepare for a next step
    stepsM
            
{- For a set of instances of a 'SequentLike' type @s@, and two corresponding
   instances of a 'HerbrandBase' type @h@, where @h@ and @s@ implement a 
   'HerbrandImpl', also a 'Counter' state (of type Int) for naming new elements, 
   returns a new instance of @h@ containing all new facts that may be deduced. 
   Input:
   - An instance of @SequentMap s@, containing sequents that are relevant to
   the last set of changes (delta base). 
   - An instance of @h@, the current Herbrand base (set of deduced facts). 
   - An instance of @h@, containing the last set of changes, i.e. delta base. 
   - Provenance information for the existing base and delta.
   - The state of the 'Counter' associated to the current run of the Chase. 
   - The propositional theory in the current run of the Chase.
   
   Output: 
   - A new base, containing the set of facts that may be deduced.
   - New provenance information that includes provenance information for the
   new set of facts as well. 
   - A new state for the 'Counter' associated to the current run of the Chase.-}
iterateBalance :: (HerbrandImpl h s r, SATAtom t) => SequentMap s -> h 
               -> h -> ProvInfo -> Int -> SATTheory t 
               -> Config -> (h, ProvInfo, Int, SATTheory t)
iterateBalance seqs base dlt provs cnt propThy cfg = 
    -- trace "*********************************"
    -- trace "Base:"              
    -- traceShow (baseSize base)
    -- trace "Delta:"
    -- traceShow (baseSize dlt)
    -- $
    let uni = unionBases base dlt
    in  Map.foldlWithKey' 
            (\(n, p, c, pt) k s -> balance k s base dlt uni n p c pt cfg) 
            (emptyBase, provs, cnt, propThy) seqs

{- As a helper for iterateBalance, returns all the new information that may be
   deduced by balancing a single sequent in the current state of base and delta.

   Input:
   - The sequent of type @s@ to balance.
   - A base of type @h@, containing the facts that have been deduced so far in
   the current base.
   - A "delta" base of type @h@, containing the last set of changes that were 
   deduced. The combination of the current base and the delta makes incremental
   chase possible.
   - A "uni" base of type @h@, containing all the facts in the original base and
   in the delta base.
   - A "new" base of type @h@, containing all the facts that have been deduced
   in the current iteration (initiated by 'iterateBalance').
   - Provenance information of type 'ProvInfo' for the facts and elements that
   have been deduced so far.
   - The state of 'Counter' for the current run of the Chase.
   - The current propositional theory in this run of the Chase.

   Output:
   - A base of type @h@, containing the facts deduced for the input sequent and
   the facts in the base "new", which was passed as input.
   - Updated provenance information of type 'ProvInfo'.
   - New state for the Chase's 'Counter'.
   - New propositional theory after pushing new information
 -}
balance :: (HerbrandImpl h s r, SATAtom t) => 
           Id -> s -> h -> h -> h -> h -> ProvInfo -> Int
           -> SATTheory t -> Config -> (h, ProvInfo, Int, SATTheory t)
balance id seq base dlt uni new provs cnt propThy cfg = 
    let res = -- traceShow "----------"
              -- traceShow (toSequent seq)
              -- traceShow base
              -- traceShow dlt
              -- traceShow uni
              -- traceShow provs
              -- traceEval
              -- $
              evalPullM (pull seq base dlt) uni provs
    in  runPushM (push seq res new) uni (id, provs) cnt propThy cfg