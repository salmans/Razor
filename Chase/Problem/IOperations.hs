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

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities
import Tools.GeoUnification
--import Tools.Narrowing

-- Chase Modeuls:
import Chase.Problem.BaseTypes
import Chase.Problem.Observation
import Chase.Problem.Structures
import Chase.Problem.Model (Model(..), modelDomain)
import qualified Chase.Problem.Model as Model
import Chase.Problem.RelAlg.RelAlg
import qualified Chase.Problem.RelAlg.Operations as OP
import qualified RelAlg.DB as DB -- Salman: Do not import this here!
-- Other Modules
import Debug.Trace


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
    where newFrames = zipWith (\(Frame _ body head vars seq) id -> 
                                   (Frame id body head vars seq))
                      frames [(last + 1)..]
          newLastID = last + (length newFrames)

{-| Selects a problem from a pool of problems and returns the selected problem. 
  The function changes the current state of the pool of problems.
  If the pool is empty, the function returns Nothing. Otherwise, returns the 
  problem wrapped in Just.
-}
selectProblem :: ProbPool (Maybe Problem)
selectProblem =
    State.get >>= (\probs ->
    case probs of
      []   -> return Nothing
      p:ps -> State.put ps >>= (\_ -> 
              return $ Just p))

{-| Inserts a problem into the problem pool. -}
scheduleProblem :: Problem -> ProbPool ()
scheduleProblem = \p -> State.get  >>= (\ps ->
                  -- Reschedule the problem at the head of the pool
                  State.put (p:ps) >>= (\_ -> 
                  return ()))


{-| Creates a Frame from a given sequent. 
-}
buildFrame :: ID -> Sequent -> Frame
buildFrame id sequent@(Sequent body head) = 
    Frame { frameID      = id
          , frameBody    = (processBody body) 
          , frameHead    = (processHead head) 
          , frameVars    = (union (freeVars head) (freeVars body))
          , frameRelInfo = RelInfo bdyInf (bdyDlt, bdyLbls) hdInf}
    where bdyInf@(bdyExp, bdyLbls) = bodyRelExp body
          hdInf                    = headRelExp head
          bdyDlt                   = delta bdyExp

{- Constructs the head of a frame from the head of a sequent. Here, we assume 
that the sequent is in the standard form, i.e. disjunctions appear only at 
the top level of the head.
-}
processHead :: Formula -> [[Obs]]
processHead Fls = []
processHead (And p q) = 
    let p' = processHead p
        q' = processHead q        
    in case (p',q') of
         -- Because disjunction appears only at the top level, the result of
         -- applying this function to a conjunct must be a list with only one
         -- element:
         ([], []) -> []
         ([p''], [q'']) -> [p'' ++ q'']
         ([p''], []) -> [] -- [p'']
         ([], [q'']) -> [] -- [q'']
         otherwise -> error err_ChaseProblemOperations_DisjTop 
processHead (Or p q) = filter (not.null) $ processHead p ++ processHead q
processHead (Exists x p) = processHead p
processHead (Atm (R "=" [t1, t2])) = [[Eql t1 t2]] -- dealing with equality
-- The following would never happen unless something is wrong with the parser:
processHead (Atm (R "=" _)) = error err_ChaseProblemOperations_EqTwoParam
processHead (Atm atm) = [[Fct atm]] -- atoms other than equality
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
matchFrame :: Tables -> Tables -> RelInfo -> [Sub]
matchFrame tbls delts relInfo@(RelInfo bodyInfo bodyDiffInfo headInfo) =
    let bdySet      = evaluateRelExp tbls delts bdyExp
        hdSetLabels = (\(e, l) -> (evaluateRelExp tbls emptyTables e, l)) 
                      <$> headInfo
        facts       = diff (bdySet, bdyLbls) hdSetLabels
    in  createSubs bdyLbls facts
    where (bdyExp, bdyLbls) = bodyDiffInfo
          noFVars lbls  = all (\l -> ((not.isJust) l) ||
                                           not(l `elem` bdyLbls)) lbls

{- A helper for matchFrame -}
createSubs :: Labels -> Table -> [Sub]
createSubs vars (DB.Set [])  = []
createSubs vars (DB.Set set) = Map.fromList.subList <$> set
    where subList ts = [(fromJust v, t) | (v, t) <- zip vars ts, isJust v]

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