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

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities
import Tools.GeoUnification
import Tools.Narrowing

-- Chase Modeuls:
import Chase.Problem.Observation
import Chase.Problem.Structures
import Chase.Problem.Model (Model(..))
import qualified Chase.Problem.Model as Model
import qualified CC.CC as CC
import Chase.Problem.IRelAlg
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
{-| Creates a problem corresponding to a given geometric theory.
-}
buildProblem :: Theory -> Problem
buildProblem thy = problem
    -- State.put [problem] >>= \_ -> return ()
    where frms = zipWith (\x y -> buildFrame x y) [1..] thy
          -- convert sequents to frames and assign IDs to them.
          problem =     Problem { problemFrames       = frms 
                                , problemModel        = Model.empty 
                                , problemQueue        = [] 
                                , problemSymbols      = framesSymbolMap frms
                                , problemLastID       = length thy 
                                , problemLastConstant = 0}


{-| Updates a problem by adding new frames to its theory. It updates problem's symbol map accordingly.
-}
extendProblem :: Problem -> [Frame] -> Problem
extendProblem (Problem oldFrames model queue symMap last lastConst) 
              frames =
    Problem { problemFrames       = (union oldFrames newFrames) 
            , problemModel        = model 
            , problemQueue        = queue 
            , problemSymbols      = newSymMap
            , problemLastID       = newLastID
            , problemLastConstant = lastConst}
        -- REMARK: the two set of frames have to be unined; otherwise, the chase may
        -- never terminate (as for thyphone1_2)
    where newFrames = zipWith (\(Frame _ body head vars seq) id -> 
                                   (Frame id body head vars seq))
                      frames [(last + 1)..]
          newLastID = last + (length newFrames)
          newSymMap = Map.unionWith (++) symMap $ framesSymbolMap newFrames

{-| Selects a problem from a pool of problems and returns the selected problem. The function changes the current state of the pool of problems.
If the pool is empty, the function returns Nothing. Otherwise, returns the problem wrapped in Just.
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


{- Builds a symbol map for a list of frames. -}
framesSymbolMap :: [Frame] -> SymbolMap
framesSymbolMap [] = Map.empty
framesSymbolMap frms =
    foldr1 (\f res -> Map.unionWith (++) res f) mapList
    -- Let's fold the SymbolMap of all frames to construct a SymbolMap for all
    where mapList = map symMap frms
          -- mapListis a list of SymbolMaps for all of the input frames.
          symMap f = Map.fromListWith (++) [(s, [(frameID f, n)])| 
                                            (n, ss) <- syms f,
                                            s <- ss]
          -- (symMap f) constructs a SymbolMap for f
          syms f   = zip [0..] $ map obsFuncSyms $frameBody f
          -- (syms f) is a list of symbols in every obs of f's body together
          -- with the ordinal of the body in f.

{-| Creates a Frame from a given sequent. 
-}
buildFrame :: ID -> Sequent -> Frame
buildFrame id sequent@(Sequent body head) = 
    Frame { frameID = id
          , frameBody = (processBody body) 
          , frameHead = (processHead head) 
          , frameVars = (union (freeVars head) (freeVars body))
          , frameOrig = sequent}


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
processBody :: Formula -> [Obs]
processBody Tru = []
processBody (And p q) = processBody p ++ processBody q
processBody (Atm (R "=" [t1, t2])) = [Eql t1 t2] -- dealing with equality
processBody (Atm (R "=" _)) = error err_ChaseProblemOperations_EqTwoParam
processBody (Atm atm) = [Fct atm] -- atoms other than equality
processBody _ = error err_ChaseProblemOperations_InvldSeq

{-| Returns True if the input set of observations are true in the given mode, otherwise False. Vars is the set of universally quantified variables in the observations while the other variables in the formula are existentially quantified (the same contract as for Frame). -}
holds :: Model -> Vars -> [Obs] -> Bool
holds _ _ [] = True
holds model@(Model _ domain) vars allObs@(obs:rest)
      | null fvars = Model.isTrue model obs && (holds model vars rest)
      -- if the formula is closed, check if it is true in the model
      | null (fvars `intersect` vars) =
          let makeSub      = Map.singleton $ head fvars
              liftAllObs e = map ((liftTerm.lift) (makeSub e)) allObs
          in or $ map (\e -> holds model vars (liftAllObs e)) domain 
      -- If the formula is not closed, instantiate the first for one of the
      -- instantiations of the first existential variable, it has to be true.
      | otherwise = error err_ChaseProblemOperations_OpenFmla
    where fvars = freeVars obs

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
formulaHolds model atom@(Atm atm@(R sym terms)) =
    Model.isTrue model obs
    where obs = termToObs True $ (fromJust.toTerm) atm --not great!!
formulaHolds model (Or p q) = fmlaHolds p || fmlaHolds q
    where fmlaHolds = formulaHolds model
formulaHolds model (And p q) = fmlaHolds p && fmlaHolds q
    where fmlaHolds = formulaHolds model
formulaHolds model@(Model trs domain) (Exists x p) = 
    let makeSub    = Map.singleton x
        liftWith e = (liftTerm.lift) (makeSub e) p
    in or $ map ((formulaHolds model).liftWith) domain

{-| Applies narrowing on a given observation to with respect to a set of rewrite rules
  and returns a set of substitutions.
-}
narrowObs :: Model -> [CC.RWRule] -> Obs -> [Sub]
narrowObs mdl@(Model trs _) rules (Den t) =
    error err_ChaseProblemOperations_NarrowDen
narrowObs mdl@(Model trs _) rules (Eql t1 t2) =
    if null subs
       then narrowEquation False trs rules t2 t1
       else subs
    where subs = narrowEquation False trs rules t1 t2
narrowObs mdl@(Model trs _) rules f@(Fct a) =
    map snd $ narrowTerm True trs rules t
    where t = fromJust $ toTerm a