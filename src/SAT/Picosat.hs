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

  Module      : SAT.Picosat
  Description : The module provides an interface to Picosat.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module SAT.Picosat where


import Picosat

-- Standard
import Data.List (intercalate)
import qualified Data.Bimap as Bimap
import Data.Maybe

-- Control
import Control.Applicative
import Control.Monad.ST
import Control.Monad.State

-- Common
import Common.Observation (Observation, ObservationSequent (..))

-- SAT
import SAT.Data

-- Tools
import qualified Tools.ExtendedSet as ExSet

{-|Solver Interface Types -}
type SATTheoryType = PropTheory
type SATInfoType   = (ClauseSet, ObsPropMap)


{-| A proposition is simply an index, which represent 'Observation's in
  'PropSequent's. -}
type Prop = Int

instance SATAtom Prop where
    emptySATTheory = emptyPropTheory
    storeSequent   = addToPropTheory

{-| A clause is a list of propositions of type 'Prop' -}
type Clause = [Prop]

{-| A clause set is a list of clauses of type 'Clause' -}
type ClauseSet = [Clause]

type PropSequent = SATSequent Prop

propSequentBody :: PropSequent -> [Prop]
propSequentBody  = satSequentBody

propSequentHead :: PropSequent -> [[Prop]]
propSequentHead  = satSequentHead

{- ObsPropMap is a bidirectional map from 'Observation' to a 'Prop'. This is 
   used to convert an 'ObservationSequent' to 'PropSequent' and vice versa. -}
type ObsPropMap = Bimap.Bimap Observation Prop

{-| A propositional theory is a set of instances of the input geometric 'Theory' 
  that is passed to the SAT solver. 

  [@propTheorySequents@] is a set of 'PropSequent's, which are propositional 
  instances of the input theory. 
  [@propTheoryMap@] is a map from -}
data instance SATTheory Prop = PropTheory 
    { propTheorySequents :: ExSet.Set PropSequent
    , propTheoryMap      :: ObsPropMap
    , lastIndex          :: Int }

type PropTheory = SATTheory Prop


{- Show instance for 'PropTheory' -}
instance Show (SATTheory Prop) where
    show (PropTheory seqs opMap ind) =
        "Sequents :" ++ "\n" ++ (show seqs) ++ "\n"
        ++ "Map: " ++ (show opMap)
        ++ "\n" ++ show ind

{-| Empty 'PropTheory' -}
emptyPropTheory = PropTheory ExSet.empty Bimap.empty 1

{-| Adds (after converting to 'PropSequent') an 'ObservationSequent' to an 
  existing 'PropTheory'. -}
addToPropTheory :: PropTheory -> ObservationSequent -> PropTheory
addToPropTheory (PropTheory consts opMap ind) (ObservationSequent fBody fHead) = 
    let (ind' , bodyProps, opMap' ) = foldr obsFoldFunc (ind , [], opMap ) fBody
        (ind'', headProps, opMap'') = foldr headFoldFunc (ind', [], opMap') fHead
        const = SATSequent bodyProps headProps
    in PropTheory (ExSet.insert const consts) opMap'' ind''
    where obsFoldFunc o (i, ps, m) = 
              let (i', p, m') = insertLookup i o m
              in  (i', p:ps, m')
          headFoldFunc os (i, pss, m) =
              let (i', ps, m') = foldr obsFoldFunc (i, [], m) os
              in  (i', ps:pss, m')
          
{- A helper for addToPropTheory -}
insertLookup :: Int -> Observation -> ObsPropMap -> (Int, Prop, ObsPropMap)
insertLookup ind obs oldMap = 
    let oldVal = Bimap.lookup obs oldMap
        newVal = ind
    in  case oldVal of
          Nothing -> (ind + 1, newVal, Bimap.insert obs newVal oldMap)
          Just v  -> (ind    , v     , oldMap)

instance SATSolver Prop (ClauseSet, ObsPropMap) where
    satInitialize = translatePropTheory
    satSolve      = undefined -- oneSolution


{-| Translates the input propositional theory to a 'ClauseSet' that can be fed 
  to the solver. It also provides the 'ObsPropMap' required for translation. -}
translatePropTheory :: PropTheory -> (ClauseSet, ObsPropMap)
translatePropTheory (PropTheory seqs propMap _) = 
    let clauses = concatMap translatePropSequent $ ExSet.toList seqs
    in  (clauses, propMap)

{- A helper for 'translatePropTheory', which converts a single 'PropSequent'. -}
translatePropSequent :: PropSequent -> [Clause]
translatePropSequent (SATSequent bdy hd) = 
    let bdy' = (\b -> - b) <$> bdy
    in  (bdy' ++) <$> (cnfHelper hd)

{- A helper for 'translatePropSequent' -}
cnfHelper :: [[Int]] -> [Clause]
cnfHelper []      = [[]]
cnfHelper [l]     = [[p] | p <- l]
cnfHelper ([]:ls) = cnfHelper ls
cnfHelper (l:ls)  = [ p:q | p <- l, q <- cnfHelper ls]

{-| Converts a 'Solution' computed by the SAT-solver to a list of 
  'Observation's, forming a model. 
  Inputs: 
  - an instance of 'ObsPropMap' that maps the propositions in the solutio to 
  first-order 'Observation's. 
  - an instance of 'Solution', computed by the SAT-solver.

  Output: 
  - A list of 'Observation's, forming the first-order model. -}
translateSolution :: ObsPropMap -> Solution -> [Observation]
translateSolution _ Unknown            = error "Unknown Solution!"
translateSolution _ Unsatisfiable      = error "Unsatisfiable Solution!"
translateSolution poMap (Solution sol) = 
    fromJust <$> ((\s -> Bimap.lookupR s poMap) <$> (filter (>0) sol))


{-| Computes all the solutions to a set of input clauses.

  Input:
  - an instance of 'ClauseSet' containing the propositional satisfiability 
  problem.
  Output:
  - an list of 'Solution's. -}
allPropSolutions :: (ClauseSet, ObsPropMap) -> [[Observation]]
allPropSolutions (clauses, propMap) = 
    translateSolution propMap <$> (filter (/= Unsatisfiable) 
                                              $ runST $ allSolutionsST clauses)

{-| Computes only one solution to a set of input clauses.

  Input:
  - an instance of 'ClauseSet' containing the propositional satisfiability
  problem.

  Output:
  - a 'Solution' instance.
 -}
oneSolution :: (ClauseSet, ObsPropMap) -> Maybe [Observation]
oneSolution = listToMaybe.allPropSolutions

-- Inside ST Monad
allSolutionsST :: ClauseSet -> ST t [Solution]
allSolutionsST fmla = allSolutionsHelper fmla []

allSolutionsHelper :: ClauseSet -> [Solution] -> ST t [Solution]
allSolutionsHelper fmla sols = do
  sol   <- nextSolution fmla sols
  case sol of
    Unknown       -> return [Unknown]
    Unsatisfiable -> return [Unsatisfiable]
    otherwise     -> do
             sols' <- allSolutionsHelper fmla (sol:sols)
             return (sol:sols')


nextSolution :: ClauseSet -> [Solution] -> ST t Solution
nextSolution fmla sols = do
  let pos = (\sol -> case sol of 
                       Solution s    -> [-p | p <- s, p > 0]
                       Unknown       -> []
                       Unsatisfiable -> []) <$> sols
  minimize (fmla ++ pos)
  

{-| Computes a minimum 'Solution' using an Aluminum-like algorithm. -}
minimize :: ClauseSet -> ST t Solution
minimize fmla = do
  solution <- solveST fmla
  reduce fmla solution

{- A helper for minimize -}
reduce :: ClauseSet -> Solution -> ST t Solution
reduce _ Unknown                    = return Unknown
reduce _ Unsatisfiable              = return Unsatisfiable
reduce fmla solution@(Solution sol) = do
    let pos = [[p] | p <- sol, p < 0]
    let neg = [-p   | p <- sol, p > 0]
    let fmla' = fmla ++ neg:pos
    solution' <- solveST fmla'
    case solution' of
      Unsatisfiable -> return solution 
      Unknown       -> return solution
      otherwise     -> reduce fmla solution'

-- Inside IO monad:
-- oneSolution :: ClauseSet -> IO Solution
-- oneSolution fmla = do
--   sols <- allSolutions fmla
--   return $ head sols

-- allSolutions :: ClauseSet -> IO [Solution]
-- allSolutions fmla = allSolutionsHelper fmla []

-- allSolutionsHelper :: ClauseSet -> [Solution] -> IO [Solution]
-- allSolutionsHelper fmla sols = do
--   sol   <- nextSolution fmla sols
--   case sol of
--     Unknown       -> return [Unknown]
--     Unsatisfiable -> return [Unsatisfiable]
--     otherwise     -> do
--              sols' <- allSolutionsHelper fmla (sol:sols)
--              return (sol:sols')


-- nextSolution :: ClauseSet -> [Solution] -> IO Solution
-- nextSolution fmla sols = do
--   let pos = map (\sol -> case sol of 
--                            Solution s -> [-p | p <- s, p > 0]
--                            Unknown    -> []
--                            Unsatisfiable -> []) sols
--   minimize (fmla ++ pos)
  


-- minimize :: ClauseSet -> IO Solution
-- minimize fmla = do
--   solution <- solve fmla
--   reduce fmla solution


-- reduce :: ClauseSet -> Solution -> IO Solution
-- reduce _ Unknown                    = return Unknown
-- reduce _ Unsatisfiable              = return Unsatisfiable
-- reduce fmla solution@(Solution sol) = do
--     let pos = [[p] | p <- sol, p < 0]
--     let neg = [-p   | p <- sol, p > 0]
--     let fmla' = fmla ++ neg:pos
--     solution' <- solve fmla'
--     case solution' of
--       Unsatisfiable -> return solution 
--       Unknown       -> return solution
--       otherwise     -> reduce fmla solution'
