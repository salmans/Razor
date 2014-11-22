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
-- import Control.Monad.Loops
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

{-| Runs the Chase algorithm and returns the resulting base, the resulting 
  provenance information and the resulting set of ground sequents.
  Input: 
  - A 'Config' instance, providing user preferences. 
  - An instance of 'SequentMap' for a 'SequentLike' type @s@. 

  Output:
  A triple of a base of type @h@, its corresponding provenance information of
  type 'ProvInfo', and a set of ground sequents of type 'SATTheory t' -}
chase :: (ChaseImpl h s r, SATAtom t) => Config -> SequentMap s 
      -> (h, ProvInfo, SATTheory t, Int)
chase cfg seqs = 
    let prob            = Problem seqs emptyBase emptyBase emptyProvInfo emptySATTheory
        (b, p, t, _, c) = runChase cfg 0 prob $ initM >> stepsM
    in  (b, p, t, c)


{-| Resumes the Chase algorithm on a partially computed set of structures and
  returns the resulting base, the resulting provenance information and the 
  resulting set of ground sequents.
  Input: 
  - A 'Config' instance, providing user preferences. 
  - An instance of 'SequentMap' for a 'SequentLike' type @s@. 
  - An instance of a PossibleFacts type @h@ as the initial base. Type @h@ must
  implement a ChaseImpl instance that matches with the type of the sequents 
  of the input 'SequentMap'.
  - Another instance of the same PossibleFacts type @h@ as the delta set.
  - Initial provenance information.
  - Initial ground theory of type @SATTheory t@

  Output:
  A triple of the new base of type @h@, its corresponding provenance information
  of type 'ProvInfo', and the new set of ground sequents of type 'SATTheory t'. 
 -}
resumeChase :: (ChaseImpl h s r, SATAtom t) => Config -> Int -> SequentMap s
            -> h -> h -> ProvInfo -> SATTheory t -> (h, ProvInfo, SATTheory t, Int)
resumeChase cfg cnt seqs base delt provs propThy = 
    let prob             = Problem seqs base delt provs propThy
        (b, p, pt, _, c) = runChase cfg cnt prob $ initM >> stepsM
    in  (b, p, pt, c)

{-| Runs the Chase algorithm, ensuring that the initial set of constants will
  denote elements in the output instnace of 'PossibleFacts'. -}
chaseWithInitialConstants :: (ChaseImpl h s r, SATAtom t) => Config -> Int
                          -> SequentMap s -> [Constant] 
                          -> (h, ProvInfo, SATTheory t, Int)
chaseWithInitialConstants cfg cnt seqs consts = 
    let base             = emptyBaseWithConstants consts
        provs            = emptyProvInfoWithConstants consts
        prob             = Problem seqs base emptyBase provs emptySATTheory
        (b, p, pt, _, c) = runChase cfg cnt prob $ initM >> stepsM
    in  (b, p, pt, c)

{- Runs a monadic function of type 'ChaseM h s ()' based on instances of type 
   @h@ and @s@ and @r@ that implement a 'ChaseImpl'.
  Input:
  - An instance of 'Config' specifying user preferences.
  - An instance of the initial problem of type @Problem h s t@.
  - The monadic function to run the chase.
  
  Output:
  A tuple containing the following:
  - An instance of type @h@, containing all the deduced facts based on the 
  initial instances and the initial configuration.
  - An instance of 'ProvInfo' that contains the provenance information for the
  facts and the elements of the output base.
  - An instance of 'SATTheory t' as the new instance of ground sequents.
  - Log information.
-}
runChase :: (ChaseImpl h s r, SATAtom t) => Config -> Int -> Problem h s t
          -> ChaseM h s () -> (h, ProvInfo, SATTheory t, [String], Int)
runChase cfg counter prob context = 
    let runCM       = RWS.execRWST context [] prob
        runCt       = State.runStateT runCM counter
        ((p, l), c) = State.evalState runCt cfg
    in (problemBase p, problemProvs p, problemSATTheory p, l, c)


{- Initializes a run of the Chase inside a 'ChaseM' context for some 
   implementation of the Chase implemented as 'ChaseImpl'. The function 
   processes the initial sequents (sequents with empty body) only and removes 
   them from the input set of sequents map.
 -}
initM :: ChaseImpl h s r => ChaseM h s ()
initM = do
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
  liftChaseMState $ RWS.put 
        $ Problem restSeqs base (unionBases dlt newDlt) newProvs propThy'
     -- Update the computation state with new base and delta base. 
     -- Also, throw away the starting sequents

{- As a helper for 'chase', iterates through the sequents of the theory (with 
   non-empty bodies) recursively until the changes in one iteration is contained
   by the existing base. 
 -}
stepsM :: ChaseImpl h s r => ChaseM h s ()
stepsM = do
  cfg <- liftChaseMConfig State.get
  Problem seqs base dlt provs propThy <- liftChaseMState (RWS.get)
                                         
  if nullBase dlt -- If the current delta contains nothing new
  then return ()
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
   instances of a 'PossibleFacts' type @h@, where @h@ and @s@ implement a 
   'ChaseImpl', also a 'Counter' state (of type Int) for naming new elements, 
   returns a new instance of @h@ containing all new facts that may be deduced. 
   Input:
   - An instance of @SequentMap s@, containing sequents that are relevant to
   the last set of changes (delta base). 
   - An instance of @h@, the current base (set of deduced facts). 
   - An instance of @h@, containing the last set of changes, i.e. delta base. 
   - Provenance information for the existing base and delta.
   - The state of the 'Counter' associated to the current run of the Chase. 
   - The ground theory in the current run of the Chase.
   
   Output: 
   - A new base, containing the set of facts that may be deduced.
   - New provenance information that includes provenance information for the
   new set of facts as well. 
   - A new state for the 'Counter' associated to the current run of the Chase.-}
iterateBalance :: (ChaseImpl h s r, SATAtom t) => SequentMap s -> h 
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
   - The current ground theory in this run of the Chase.

   Output:
   - A base of type @h@, containing the facts deduced for the input sequent and
   the facts in the base "new", which was passed as input.
   - Updated provenance information of type 'ProvInfo'.
   - New state for the Chase's 'Counter'.
   - New ground theory after pushing new information
 -}
balance :: (ChaseImpl h s r, SATAtom t) => 
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