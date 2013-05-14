{-| Time-stamp: <2013-02-22 01:08:17 Salman Saghafi>

   This module contains basic operations related to a problem structure. 

-}
module Chase.Problem.IOperations where


-- General Modules
import Data.List
import qualified Data.Map as Map

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
buildProblem thy =
    Problem frms Model.empty (Queue []) (framesSymbolMap frms) 
                (ID (length thy)) 0
    where frms = zipWith (\x y -> buildFrame (ID x) y) [1..] thy

{-| Updates a problem by adding new frames to its theory. It updates problem's symbol map accordingly.
-}
extendProblem :: Problem -> [Frame] -> Problem
extendProblem (Problem oldFrames model queue symMap lastID@(ID last) lastConst) frames =
    Problem (union oldFrames newFrames) model queue newSymMap newLastID lastConst
    where newFrames = zipWith (\(Frame _ body head vars) id -> 
                                   (Frame (ID id) body head vars)) 
                      frames [(last + 1)..]
          newLastID = ID $ last + (length newFrames)
          newSymMap = Map.unionWith (++) symMap $ framesSymbolMap newFrames

{-| Selects a problem from a pool of problems and returns the selected problem together with the rest of the problems. For now, we choose the first problem in the list in a FIFO fashion.
-}
selectProblem :: [Problem] -> (Problem, [Problem])
selectProblem [] = error err_ChaseProblemOperations_NoProb
selectProblem probs = (head probs, tail probs) -- FIFO for now

{-| Combines a problem into a list of problems. The position of the problem in the list can be an important parameter for selectProblem; thus, it is a scheduling procedure. The current scheduling procedure is FIFO.
-}
scheduleProblem :: Problem -> [Problem] -> [Problem]
scheduleProblem prob probs = prob:probs -- probs ++ [prob] -- FIFO for now


{- Builds a symbol map for a list of frames. -}
framesSymbolMap :: [Frame] -> SymbolMap
framesSymbolMap [] = Map.empty
framesSymbolMap (frm:frms) =
    Map.unionWith (++) 
       -- Symbols within frame frm:       
       (Map.fromListWith (++) [(s, [(frameID frm, n)])| 
                               (n, ss) <- syms, s <- ss])
       -- Symbols of the remaining frames:
       (framesSymbolMap frms)
    where syms = zip [0..] $ map obsFuncSyms $frameBody frm
    -- Syms is a list of symbols in every obs of the body and together
    -- with the ordinal of the body.

{-| Creates a Frame from a given sequent. 
-}
buildFrame :: ID -> Sequent -> Frame
buildFrame id sequent@(Sequent body head) = 
    Frame id (processBody body) (processHead head) 
              (union (fv head) (fv body))


{- Constructs the head of a frame from the head of a sequent. Here, we assume 
that the sequent is in the standard form, i.e. disjunctions appear only at 
the top level of the head.
-}
processHead :: Formula -> [[Obs]]
processHead Fls = []
processHead (And p q) = 
    case (p',q') of
      -- Because disjunction appears only at the top level, the result of
      -- applying this function to a conjunct must be a list with only one
      -- element:
      ([], []) -> []
      ([p''], [q'']) -> [p'' ++ q'']
      ([p''], []) -> [p'']
      ([], [q'']) -> [q'']
      otherwise -> error err_ChaseProblemOperations_DisjTop 
    where p' = processHead p
          q' = processHead q
processHead (Or p q) = 
    filter (\h -> not (null h)) (processHead p) ++ (processHead q)
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
processBody (And p q) = (processBody p) ++ (processBody q)
processBody (Atm (R "=" [t1, t2])) = [Eql t1 t2] -- dealing with equality
processBody (Atm (R "=" _)) = error err_ChaseProblemOperations_EqTwoParam
processBody (Atm atm) = [Fct atm] -- atoms other than equality
processBody _ = error err_ChaseProblemOperations_InvldSeq

{-| Returns true if the queue of the problem is empty; otheriwse, returns false.
-}
emptyQueue :: Problem -> Bool
emptyQueue p = null q
    where Queue q = problemQueue p

{-| Applies a function (Obs -> Obs) to the body and ([Obs] -> [Obs]) to the head of a frame and returns the frame after the application of these two functions.
-}
onFrame :: (Obs -> Obs) -> (Obs -> Obs) -> Frame -> Frame
onFrame bodyFunc headFunc (Frame id body head vars) = 
    Frame id (map bodyFunc body) (map (map headFunc) head) vars

{-| Returns true if the input Obs can be observed in the given model.
-}
isObserved :: Model -> Obs -> Bool
isObserved model obs = Model.isTrue model obs

{-| Returns the term in a model to which the input obs denotes inside an Obs wrapper.
-}
denotes :: Model -> Obs -> Obs
denotes model obs = Model.denotes model obs

{-| Returns True if the input set of observations are true in the given mode, otherwise False. Vars is the set of universally quantified variables in the observations while the other variables in the formula are existentially quantified (the same contract as for Frame). -}
holds :: Model -> Vars -> [Obs] -> Bool
holds _ _ [] = True
holds model@(Model trs domain) vars allObs@(obs:rest)
      | null fvars = isObserved model obs && (holds model vars rest)
      -- if the formula is closed, check if it is true in the model
      | null (fvars `intersect` vars) =
          or $ map 
                  (\e -> 
                       holds model vars (map ((onObs.lift) (makeSub e)) allObs))
                  domain 
      -- if the formula is not closed, instantiate the first for one of the
      -- instantiations of the first existential variable, it has to be true.
      | otherwise = error err_ChaseProblemOperations_OpenFmla
    where fvars = fv obs
          makeSub elem = Map.singleton (head fvars) elem

{-| Returns true if the sequent is true under the given model.
-}
sequentHolds :: Model -> Sequent -> Bool
sequentHolds model (Sequent b h) =
    (not (formulaHolds model b)) || (formulaHolds model h)

{-| Returns true if the formula is true under a given model.
-}
formulaHolds :: Model -> Formula -> Bool
formulaHolds _ Fls = False
formulaHolds _ Tru = True
formulaHolds model atom@(Atm atm@(R sym terms)) 
    | (all closedTerm terms) && cond = True
    | otherwise = False
    where cond = isObserved model (termToObs True (atomToTerm atm)) --not great!!
formulaHolds model (Or p q) = 
    holdsP || holdsQ
    where holdsP = formulaHolds model p
          holdsQ = formulaHolds model q
formulaHolds model (And p q) = 
    holdsP && holdsQ
    where holdsP = formulaHolds model p
          holdsQ = formulaHolds model q
formulaHolds model@(Model trs domain) (Exists x p) = 
    or $ map (\e -> formulaHolds model (onFormula (lift (makeSub e)) p)) domain
    where makeSub elem = Map.singleton x elem

{-| Adds a list of obs to the given model and returns a new model containing the new obs.
-}
addToModel :: Model -> [Obs] -> (Model, [CC.RWRule])
addToModel model obs = Model.add model obs

{-| Applies narrowing on a given observation to with respect to a set of rewrite rules
  and returns a set of substitutions.
-}
narrowObs :: Model -> [CC.RWRule] -> Obs -> [Sub]
narrowObs mdl@(Model trs _) rules (Den t) =
    error err_ChaseProblemOperations_NarrowDen
narrowObs mdl@(Model trs _) rules (Eql t1 t2) =
    narrowEquation False trs rules t1 t2
narrowObs mdl@(Model trs _) rules f@(Fct a) =
    [s | (_, s) <- narrowTerm True trs rules t, 
                   CC.normalForm trs (lift s t) == truth]
    where t = atomToTerm a
-- A less efficient version of narrowing:
-- narrowObs mdl@(Model trs _) rules (Den t) = 
--     [s | (_, s) <- narrowTerm False trs rules t]
-- narrowObs mdl@(Model trs _) rules (Eql t1 t2) = union subs1 subs2
--     where subs1 = [s | (t1', s) <- narrowTerm False trs rules t1]
--           subs2 = [s | (t2', s) <- narrowTerm False trs rules t2]
-- narrowObs mdl@(Model trs _) rules (Fct a) = 
--     [s | (_, s) <- narrowTerm True trs rules t]
--     where t = atomToTerm a
