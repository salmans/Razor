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

  Module      : Chase.IChase
  Description : This module contains the internal implementation of the Chase.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DoAndIfThenElse #-}

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
import Tools.FolToGeo (parseFolToTheory)

{-| Runs the Chase algorithm and returns the resulting base, the resulting 
  provenance information and the resulting set of ground sequents.
  Input: 
  - A 'Config' instance, providing user preferences. 
  - An instance of 'SequentMap' for a 'SequentLike' type @s@. 

  Output:
  A tuple of a base of type @h@, its corresponding provenance information of
  type 'ProvInfo', an instance of 'SATIterator' as a handle to communicate with
  the SAT/SMT solver, and a counter for the index of the last element created. -}
chase :: (ChaseImpl h s r, SATIterator it) => Config -> SequentMap s 
      -> (h, ProvInfo, it, Int)
chase cfg seqs = 
    let prob   = Problem seqs emptyBase emptyBase emptyProvInfo (satInitialize cfg)
        (b, p, it, _, c) = runChase cfg 0 prob $ initM >> stepsM
    in  (b, p, it, c)


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
  - Initial 'SATIterator' instance

  Output:
  A tuple of a new base of type @h@, its corresponding provenance information of
  type 'ProvInfo', an instance of 'SATIterator' as a handle to communicate with
  the SAT/SMT solver, and a counter for the index of the last element created. -}
resumeChase :: (ChaseImpl h s r, SATIterator it) => Config -> Int -> SequentMap s
            -> h -> h -> ProvInfo -> it -> (h, ProvInfo, it, Int)
resumeChase cfg cnt seqs base delt provs iter = 
    let prob             = Problem seqs base delt provs iter
        (b, p, it, _, c) = runChase cfg cnt prob $ initM >> stepsM
    in  (b, p, it, c)

{-| Runs the Chase algorithm, ensuring that the initial set of constants will
  denote elements in the output instnace of 'PossibleFacts'. -}
chaseWithInitialConstants :: (ChaseImpl h s r, SATIterator it) => Config -> Int
                          -> SequentMap s -> [Constant] 
                          -> (h, ProvInfo, it, Int)
chaseWithInitialConstants cfg cnt seqs consts = 
    let base             = emptyBaseWithConstants consts
        provs            = emptyProvInfoWithConstants consts
        prob             = Problem seqs base emptyBase provs (satInitialize cfg)
        (b, p, it, _, c) = runChase cfg cnt prob $ initM >> stepsM
    in  (b, p, it, c)

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
  - An instance of 'SATIterator' to communicate with the SAT/SMT solver.
  - Log information.
-}
runChase :: (ChaseImpl h s r, SATIterator it) => Config -> Int -> Problem h s it
          -> ChaseM h s () -> (h, ProvInfo, it, [String], Int)
runChase cfg counter prob context = 
    let runCM       = RWS.execRWST context [] prob
        runCt       = State.runStateT runCM counter
        ((p, l), c) = State.evalState runCt cfg
    in ( problemBase p, problemProvs p, problemSATIterator p, l, c)


{- Initializes a run of the Chase inside a 'ChaseM' context for some 
   implementation of the Chase implemented as 'ChaseImpl'. The function 
   processes the initial sequents (sequents with empty body) only and removes 
   them from the input set of sequents map.
 -}
initM :: ChaseImpl h s r => ChaseM h s ()
initM = do
  cfg <- liftChaseMConfig State.get
  Problem seqs base dlt provs iter <- liftChaseMState RWS.get
  let (startSeqs, restSeqs) = Map.partition startSequent seqs
                              -- Separate sequents with empty body
  counter <- (liftChaseMCounter State.get)
  let (newDlt, newProvs, counter', iter') = 
          iterateBalance startSeqs base dlt provs counter iter cfg
                           -- Balance the sequents with empty body right here
                           -- before iterating through the remaining sequents

  liftChaseMCounter (State.put counter') -- Update the new counter
  liftChaseMState $ RWS.put 
        $ Problem restSeqs base (unionBases dlt newDlt) newProvs iter'
     -- Update the computation state with new base and delta base. 
     -- Also, throw away the starting sequents

{- As a helper for 'chase', iterates through the sequents of the theory (with 
   non-empty bodies) recursively until the changes in one iteration is contained
   by the existing base. 
 -}
stepsM :: ChaseImpl h s r => ChaseM h s ()
stepsM = do
  cfg <- liftChaseMConfig State.get
  Problem seqs base dlt provs iter <- liftChaseMState (RWS.get)
                                         
  if nullBase dlt -- If the current delta contains nothing new
  then return ()
  else do          -- else make a new step
    let seqs' = Map.filter ((flip relevant) dlt) seqs
                -- Select sequents that are relevant to the last set of changes
    counter <- (liftChaseMCounter State.get)
    let (newDlt, newProvs, counter', iter') = 
            iterateBalance seqs' base dlt provs counter iter cfg
        -- Process the relevant sequents
    liftChaseMCounter (State.put counter')
    liftChaseMState $ RWS.put $ Problem seqs (unionBases base dlt) 
                                newDlt newProvs iter'
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
   - An instance of 'SATIterator' to interact with the underlying SAT/SMT solver
   - A 'Config' instance
   
   Output: 
   - A new base, containing the set of facts that may be deduced.
   - New provenance information that includes provenance information for the
   new set of facts as well. 
   - A new state for the 'Counter' associated to the current run of the Chase.
   - The updated iterator to interact with the SAT/SMT solver
-}
iterateBalance :: (ChaseImpl h s r, SATIterator it)
               => SequentMap s -> h -> h -> ProvInfo -> Int -> it
               -> Config -> (h, ProvInfo, Int, it)
iterateBalance seqs base dlt provs cnt iter cfg = 
    -- trace "*********************************"
    -- trace "Base:"              
    -- traceShow (baseSize base)
    -- trace "Delta:"
    -- traceShow (baseSize dlt)
    -- $
    let uni = unionBases base dlt
    in  Map.foldlWithKey' 
            (\(n, p, c, it) k s -> balance k s base dlt uni n p c it cfg) 
            (emptyBase, provs, cnt, iter) seqs

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
   - The 'SATIterator' instance
   - A 'Config' instance

   Output:
   - A base of type @h@, containing the facts deduced for the input sequent and
   the facts in the base "new", which was passed as input.
   - Updated provenance information of type 'ProvInfo'.
   - New state for the Chase's 'Counter'.
   - New 'SATIterator' instance
 -}
balance :: (ChaseImpl h s r, SATIterator it)
           => Id -> s -> h -> h -> h -> h -> ProvInfo -> Int
           -> it -> Config -> (h, ProvInfo, Int, it)
balance id seq base dlt uni new provs cnt iter cfg = 
    let res = -- traceShow "----------"
              -- traceShow (toSequent seq)
              -- traceShow base
              -- traceShow dlt
              -- traceShow uni
              -- traceShow provs
              -- traceEval
              -- $
              evalPullM (pull seq base dlt) uni provs
    in  runPushM (push seq res new) uni (id, provs) cnt iter cfg