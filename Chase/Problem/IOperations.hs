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

import Utils.Trace
import Debug.Trace
-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities
import Tools.GeoUnification
import Tools.Config

-- Chase Modeuls:
import Chase.Problem.BaseTypes
import Chase.Problem.Observation
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
err_ChaseProblemOperations_EqTwoParam = 
    "Chase.Problem.Operations.processHead: " ++
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

{-| Creates a problem corresponding to a given geometric theory. -}
buildProblem :: Theory -> Problem
buildProblem thy = problem
    where frms = zipWith (\x y -> buildFrame x y) [1..] thy
          -- convert sequents to frames and assign IDs to them.
          problem =     Problem { problemFrames       = frms 
                                , problemModel        = Model.empty 
                                , problemQueue        = []
                                , problemLastID       = length thy 
                                , problemLastConstant = 0}

{-| Updates a problem by adding new frames to its theory. It updates problem's 
  symbol map accordingly.
-}
extendProblem :: Problem -> [Frame] -> Problem
extendProblem (Problem oldFrames model queue last lastConst) 
              frames =
    Problem { problemFrames       = (union oldFrames newFrames) 
            , problemModel        = model
            , problemQueue        = queue
            , problemLastID       = newLastID              
            , problemLastConstant = lastConst}
        -- REMARK: the two set of frames have to be unined; otherwise, the chase may
        -- never terminate (as for thyphone1_2)
    where newFrames = zipWith (\frame id -> frame {frameID = id})
                      frames [(last + 1)..]
          newLastID = last + (length newFrames)

{-| Selects a problem from a pool of problems based on the given scheduling type
  and returns the selected problem. The function changes the current state of 
  the pool of problems. If the pool is empty, the function returns Nothing. 
  Otherwise, returns the problem wrapped in Just.
-}
selectProblem :: ScheduleType -> ProbPool (Maybe Problem)
selectProblem _ =
    State.get >>= (\probs ->
    case probs of
      []   -> return Nothing
      p:ps -> State.put ps >>= (\_ -> 
              return $ Just p))

{-| Inserts a problem into the problem pool based on the given scheduling 
  type. -}
scheduleProblem :: ScheduleType -> Problem -> ProbPool ()
scheduleProblem SchedFIFO = 
    \p -> State.get  >>= (\ps ->
    State.put (ps ++ [p]) >>= (\_ -> return ()))
scheduleProblem SchedFILO = 
    \p -> State.get  >>= (\ps ->
    State.put (p:ps) >>= (\_ -> return ()))

{-| Selects a frame from a set of frames and returns the selected frame as well 
  as the remaining frames. The scheduling algorithm is always fifo to maintain
  fairness. -}
{- Salman: at the moment, the only reason that we kept frames inside problems
   is because we want to maintain a different schedule for each problem. -}
selectFrame :: [Frame] -> (Maybe Frame, [Frame])
selectFrame []     = (Nothing, [])
selectFrame (f:fs) = (Just f, fs)
          -- Find the first frame that needs to be processed.

{-| Schedules a frame inside a set of frames. -}
scheduleFrame :: Frame -> [Frame] -> [Frame]
scheduleFrame f fs = fs ++ [f]

{-| Creates a Frame from a given sequent. 
-}
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
processHead Fls = ([], untypedFrame)
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
processHead (Atm (R "=" _)) = error err_ChaseProblemOperations_EqTwoParam
processHead (Atm atm) = ([[Fct atm]], untypedFrame)
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
processBody (Atm (R "=" _)) = error err_ChaseProblemOperations_EqTwoParam
processBody (Atm atm) = [Fct atm] -- atoms other than equality
processBody (Exists x p) = processBody p
processBody fmla = (error.show) fmla -- error err_ChaseProblemOperations_InvldSeq

{-| Given relational information about a sequent and a set of tables in a model,
 returns a set of substituions corresponding to a chase step for the sequent. -}
matchFrame :: Tables -> Tables -> RelInfo -> Bool -> [Sub]
matchFrame tbls 
           delts 
           relInfo@(RelInfo bodyInfo bodyDiffInfo headInfo) 
           incremental =
    let bdySet      = evaluateRelExp tbls delts bdyExp
        facts       = diff (bdySet, bdyLbls) hdSetLabels
    in  createSubs bdyLbls facts
    where (bdyExp, bdyLbls) = if incremental then bodyDiffInfo else bodyInfo
          noFVars lbls  = all (\l -> ((not.isJust) l) ||
                                           not(l `elem` bdyLbls)) lbls
          hdSetLabels = (\(e, l) -> (evaluateRelExp tbls emptyTables e, l)) 
                        <$> headInfo


{- A helper for matchFrame -}
createSubs :: Labels -> Table -> [Sub]
createSubs _ (DB.Set [])  = []
createSubs bdyVars (DB.Set set) = 
    Map.fromList.bdySubs <$> set
    where bdySubs ts = [(fromJust v, t) | (v, t) <- zip bdyVars ts, isJust v]

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
              liftAllObs e = map ((liftTerm.lift) (makeSub e)) allObs
          in or $ map (\e -> holds model vars (liftAllObs e)) domain 
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
formulaHolds model atom@(Atm atm@(F sym terms)) =
    Model.isTrue model obs
    where obs = termToObs True $ (fromJust.toTerm) atm --not great!!
formulaHolds model atom@(Atm atm@(R sym terms)) =
    Model.isTrue model obs
    where obs = termToObs True $ (fromJust.toTerm) atm --not great!!
formulaHolds model (Or p q) = fmlaHolds p || fmlaHolds q
    where fmlaHolds = formulaHolds model
formulaHolds model (And p q) = 
    fmlaHolds p && fmlaHolds q
    where fmlaHolds = formulaHolds model
formulaHolds model@(Model trs _) (Exists x p) = 
    let makeSub    = Map.singleton x
        liftWith e = (liftTerm.lift) (makeSub e) p
    in
      or $ map ((formulaHolds model).liftWith) domain
    where domain = modelDomain model