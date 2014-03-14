{-|

   This module contains basic operations related to a problem structure. 

-}
module Chase.Problem.IOperations where


-- General Modules
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Control.Monad.State as State
import qualified Control.Monad.Writer as Writer
import Control.Applicative
import Control.Monad

import Utils.Trace
import Debug.Trace
-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities
import Tools.GeoUnification
import Tools.Config as Config
import Tools.Isomorph

-- Chase Modeuls:
import Chase.Problem.BaseTypes
import Chase.Problem.Structures
import Chase.Problem.Model (Model(..), modelDomain)
import qualified Chase.Problem.Model as Model
import Chase.Problem.RelAlg.RelAlg
import qualified Chase.Problem.RelAlg.Operations as OP
import qualified RelAlg.DB as DB -- Salman: Do not import this here!


{- A list of errors raised by this module -}
err_ChaseProblemOperations_NoProb = 
    "Chase.Problem.Operations: No problems to select!"
err_ChaseProblemOperations_DisjTop = 
    "Chase.Problem.Operations.processHead: " ++
    "disjunctions can appear only at the top " ++
    "level sequents."
err_ChaseProblemOperations_processHead_EqTwoParam = 
    "Chase.Problem.Operations.processHead: " ++
    "equality must have only two parameters!"
err_ChaseProblemOperations_processBody_EqTwoParam = 
    "Chase.Problem.Operations.processBody: " ++
    "equality must have only two parameters!"
err_ChaseProblemOperations_InvldSeq = 
    "Chase.Problem.Operations.processHead: " ++
    "invliad sequent!"
err_ChaseProblemOperations_OpenFmla = 
    "Chase.Problem.Operations.holds: the input " ++
    "formula cannot have free variables."
err_ChaseProblemOperations_NarrowDen = 
    "Chase.Problem.Operations.narrowObs: narrowing " ++
    "is not defined for denotation observations."

{-| Creates an initial problem and a set of frames for a given geometric 
  theory. -}
buildProblem :: Theory -> (FrameMap, Problem)
buildProblem thy = 
    ( Map.fromList $ (\f -> (frameID f, f)) <$> frms
    , Problem { problemID           = 0
              , problemFrames       = frameID <$> frms
              , problemModel        = Model.emptyModel
              , problemQueue        = []
              , problemScheduleInfo = 
                  ScheduleInfo { problemSelectors  = allFrameTypeSelectors
                               , problemBigStepAge = 0 
                               , problemParent     = -1
                               , problemCollapses  = 0
                               , problemExtendable = True
                               , problemScore      = defaultScore }
              , problemLastConstant = 0})
    where frms  = zipWith (\x y -> buildFrame x y) [1..] thy''
          temp  = relConvert thy
          thy'  = addAllExistsPreds temp
                  -- Take existential formulas on right out of disjunctions
          thy'' = if   any hasFreeVarOnRight thy'
                       -- If any sequent has a free variable on its right
                  then addElementPred <$> thy' -- modify the theory
                  else thy'
          -- convert sequents to frames and assign IDs to them.

{- Returns true if the input sequent has any free variable on its RHS, which is 
   not defined on its LHS. -}
hasFreeVarOnRight :: Sequent -> Bool
hasFreeVarOnRight seq = (not.null) $ hdVars \\ bdyVars
    where hd      = sequentHead seq
          bdy     = sequentBody seq
          hdVars  = freeVars hd
          bdyVars = freeVars bdy

{-| Selects a problem from a pool of problems based on the given scheduling type
  and returns the selected problem. The function changes the current state of 
  the pool of problems. If the pool is empty, the function returns Nothing. 
  Otherwise, returns the problem wrapped in Just.
-}
selectProblem :: ProbPool (Maybe Problem)
-- selectProblem = do
  -- cfg <- configure Config.get
  -- let sched = configSchedule cfg
  -- (fs, probs) <- State.get
  -- if   null probs
  -- then return Nothing
  -- else do
  --   let p = maximumBy (\p p' -> 
  --                      compare ((problemScore.problemScheduleInfo) p)
  --                      ((problemScore.problemScheduleInfo) p')) probs
  --   State.put(fs, delete p probs)
  --   return (Just p) -- For now, we do score-based selection
selectProblem = 
    State.get >>= (\(fs, probs) ->
    case probs of
      []   -> return Nothing
      p:ps -> State.put (fs, ps) >>= (\_ -> 
              return $ Just p))

{-| Inserts a problem into the problem pool based on the given scheduling 
  type. -}
scheduleProblem :: Problem -> ProbPool ()
scheduleProblem p = do
  (fs, ps) <- State.get
  cfg      <- configure Config.get
  let age = (problemBigStepAge.problemScheduleInfo) p
  let ps' = deleteFirstsBy 
            (\p1 p2 -> isomorphic (problemModel p1) (problemModel p2)) ps [p]
  State.put (fs, if   age /= 0 && configIsoElim cfg
                 then scheduleProblemHelper cfg p ps'
                 else scheduleProblemHelper cfg p ps)
    
{- A helper for scheduleProblem -}
scheduleProblemHelper :: Config -> Problem -> [Problem] -> [Problem]
scheduleProblemHelper cfg p ps 
    | schedType == SchedBFS = ps ++ [p]
    | schedType == SchedDFS = p:ps
    | schedType == SchedRR  = if age `mod` unit == 0
                              then ps ++ [p]
                              else p:ps
    where schedType = configSchedule cfg
          unit      = configProcessUnit cfg
          age       = (problemBigStepAge.problemScheduleInfo) p
                        
{-| Updates the reputation of all problems in the pool with a given parent 
  using an update function. -}
updateReputation :: (Int -> Int) -> ID -> ProbPool ()
updateReputation f pid = do
  (fs, ps) <- State.get
  let ps' = (\p -> let schedInfo = problemScheduleInfo p
                       score     = problemScore schedInfo
                       reput     = scoreReputation score
                   in if   problemParent schedInfo == pid
                      then p { problemScheduleInfo = 
                                   schedInfo { problemScore = 
                                               score { scoreReputation = f reput}}}
                      else p) <$> ps
  State.put (fs, ps')

{-| Selects a frame from a set of frames and returns the selected frame as well 
  as the remaining frames. The scheduling algorithm is always fifo to maintain
  fairness. 
  The first parameter is a FrameMap, containing all the frames in the theory. 
  The second parameter is frameIDs from a problem, which contains scheduling 
  information for the problem. The third parameter is a set of selectors as 
  selecting conditions. -}
selectFrame :: FrameMap -> [ID] -> [FrameTypeSelector] -> 
               (Maybe ID, [ID])
selectFrame _ [] _      = (Nothing, [])
selectFrame _ (f:fs) [] = (Just f, fs)  -- No restriction on selected frame
selectFrame fMap fs tps = 
    case findIndex ((flip hasFrameType) tps) frames of
      Nothing -> (Nothing, fs)
      Just i  -> let (l1, l2) = splitAt i fs
                 in  (Just (head l2), l1 ++ tail l2)
    where frames = (\id -> case Map.lookup id fMap of
                             Just f -> f) <$> fs


{-| Schedules a frame inside a set of frames. -}
scheduleFrame :: FrameMap -> ID -> [ID] -> [ID]
scheduleFrame _ f fs = fs ++ [f]

{-| Creates a Frame from a given sequent. -}
buildFrame :: ID -> Sequent -> Frame
buildFrame id sequent@(Sequent body head) = 
    Frame { frameID        = id
          , frameBody      = bodies
          , frameHead      = heads
          , frameVars      = (union (freeVars head) (freeVars body))
          , frameRelInfo   = RelInfo bdyInf (bdyDlt, bdyLbls) hdInf
          , frameType      = fType { ftInitial   = null bodies 
                                   , ftUniversal = universals } }
    where bdyInf@(bdyExp, bdyLbls) 
                         = bodyRelExp body
          hdInf          = headRelExp head
          bdyDlt         = delta bdyExp
          bodies         = processBody body
          (heads, fType) = processHead head
          universals     = (not.null) $ (freeVars head) \\ (freeVars body)

{- Constructs the head of a frame from the head of a sequent. Here, we assume 
that the sequent is in the standard form, i.e. disjunctions appear only at 
the top level of the head.
-}
processHead :: Formula -> ([[Obs]], FrameType)
processHead Fls = ([], untypedFrame {ftFail = True})
processHead (And p q) = 
    let (p', ft1) = processHead p
        (q', ft2) = processHead q
        ft        = unionFrameTypes ft1 ft2
    in case (p',q') of
         -- Because disjunction appears only at the top level, the result of
         -- applying this function to a conjunct must be a list with only one
         -- element:
         ([], []) -> ([], ft)
         ([p''], [q'']) -> ([p'' ++ q''], ft)
         ([p''], []) -> ([], ft)
         ([], [q'']) -> ([], ft)
         otherwise -> error err_ChaseProblemOperations_DisjTop 
processHead (Or p q) = 
    let (p', ft1) = processHead p
        (q', ft2) = processHead q
        ft        = unionFrameTypes ft1 ft2
    in  (filter (not.null) $ p' ++ q', ft {ftDisjunction = True})
processHead (Exists x p) = 
    let (p', ft) = processHead p
    in  (p', ft { ftExistential = True})
processHead (Atm (R "=" [t1, t2])) = 
    ([[Eql t1 t2]], untypedFrame) -- dealing with equality
-- The following would never happen unless something is wrong with the parser:
processHead (Atm (R "=" _)) = 
    error err_ChaseProblemOperations_processHead_EqTwoParam
processHead (Atm atm@(R sym ts)) = 
    ([[Fct atm]], fType)
    where fType = if   all isVar ts -- Sequents with constants on right get the
                                    -- ftExistential tag.
                  then untypedFrame
                  else untypedFrame { ftExistential = True }
    -- atoms other than equality
processHead (Atm atm@(F sym ts)) = 
    ([[Fct atm]], fType)
    where fType = if   all isVar ts -- Sequents with constants on right get the
                                    -- ftExistential tag.
                  then untypedFrame
                  else untypedFrame { ftExistential = True }
    -- atoms other than equality
processHead _ = error err_ChaseProblemOperations_InvldSeq

{- Constructs the body of a frame from the body of a sequent. Here, we assume 
that the sequent is in the standard form, i.e. the body does not have 
disjunctions or existential quantifiers.
-}
-- Probably we don't need to keep frames for body. Get rid of them!
processBody :: Formula -> [Obs]
processBody Tru = []
processBody (And p q) = processBody p ++ processBody q
processBody (Atm (R "=" [t1, t2])) = [Eql t1 t2] -- dealing with equality
processBody (Atm (R "=" _)) = 
    error err_ChaseProblemOperations_processBody_EqTwoParam
processBody (Atm atm) = [Fct atm] -- atoms other than equality
processBody (Exists x p) = processBody p
processBody fmla = (error.show) fmla

{-| Given relational information about a sequent and a set of tables in a model,
 returns a set of substituions corresponding to a chase step for the sequent. -}
matchFrame :: Model -> Tables -> RelInfo -> Bool -> [Sub]
matchFrame mdl
           delts 
           relInfo@(RelInfo bodyInfo bodyDiffInfo headInfo) 
           incremental =
    let bdySet      = evaluateRelExp tbls delts bdyExp
        facts       = diff (bdySet, bdyLbls) hdSetLabels
    in  createSubs bdyLbls facts
    where tbls              = modelTables mdl
          (bdyExp, bdyLbls) = if incremental then bodyDiffInfo else bodyInfo
          noFVars lbls      = all (\l -> ((not.isJust) l) ||
                                         not(l `elem` bdyLbls)) lbls
          hdSetLabels       = (\(e, l) -> 
                                   (evaluateRelExp tbls emptyTables e, l)) 
                              <$> headInfo


{- A helper for matchFrame -}
createSubs :: Labels -> Table -> [Sub]
createSubs _ (DB.Set [])  = []
createSubs bdyVars (DB.Set set) = 
    Map.fromList.bdySubs <$> set
    where bdySubs ts = [(fromJust v, Elm t) | (v, t) <- zip bdyVars ts
                       , isJust v]

{-| Returns True if the input set of observations are true in the given mode, 
  otherwise False. Vars is the set of universally quantified variables in the 
  observations while the other variables in the formula are existentially 
  quantified (the same contract as for Frame). -}
holds :: Model -> Vars -> [Obs] -> Bool
holds _ _ [] = True
holds model vars allObs@(obs:rest)
      | null fvars = Model.isTrue model obs && (holds model vars rest)
      -- if the formula is closed, check if it is true in the model
      | null (fvars `intersect` vars) =
          let makeSub      = Map.singleton $ head fvars
              liftAllObs e = map ((liftTerm.liftSub) (makeSub e)) allObs
          in or $ map (\e -> holds model vars (liftAllObs (Elm e))) domain 
      -- If the formula is not closed, instantiate the first for one of the
      -- instantiations of the first existential variable, it has to be true.
      | otherwise = error err_ChaseProblemOperations_OpenFmla
    where fvars  = freeVars obs
          domain = modelDomain model

{-| Returns true if the sequent is true under the given model.
-}
sequentHolds :: Model -> Sequent -> Bool
sequentHolds model (Sequent b h) =
    (not.formulaHolds model) b || formulaHolds model h

{-| Returns true if the formula is true under a given model.
-}
formulaHolds :: Model -> Formula -> Bool
formulaHolds _ Fls = False
formulaHolds _ Tru = True
formulaHolds model atom@(Atm atm) =
    Model.isTrue model (atomToObs atm)
formulaHolds model (Or p q) = fmlaHolds p || fmlaHolds q
    where fmlaHolds = formulaHolds model
formulaHolds model (And p q) = 
    fmlaHolds p && fmlaHolds q
    where fmlaHolds = formulaHolds model
formulaHolds model@(Model trs _ _) (Exists x p) = 
    let makeSub    = Map.singleton x
        liftWith e = (liftTerm.liftSub) (makeSub e) p
    in  or $ map ((formulaHolds model).liftWith.Elm) domain
    where domain = modelDomain model