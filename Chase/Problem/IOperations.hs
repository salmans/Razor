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

{-| Creates an initial problem and a set of frames for a given geometric 
  theory. -}
buildProblem :: Theory -> (FrameMap, Problem)
buildProblem thy = 
    ( Map.fromList $ (\f -> (frameID f, f)) <$> frms
    , Problem { problemFrames       = frameID <$> frms
              , problemModel        = Model.empty 
              , problemQueue        = []
              , problemScheduleInfo = 
                  ScheduleInfo { problemFrameSelector = allFrameTypeSelectors
                               , problemBigStepAge    = 0 }
              , problemLastConstant = 0})
    where frms  = zipWith (\x y -> buildFrame x y) [1..] thy''
          thy'  = addAllExistsPreds thy
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

{- Since the current implementation of the chase requires every free variable
   on RHS of a sequent be mentioned on its LHS, we add a @Element predicate
   to the left of this kind of sequents. We also add @Element to the right of
   every sequent with existential quantifiers. -}
addElementPred :: Sequent -> Sequent
addElementPred seq = 
    let bdy' = foldr (\v b -> And (Atm (elementPred (Var v))) b) bdy hdFVars
        hd'  = addElementPredToRight [] hd
    in  seq { sequentBody = bdy' 
            , sequentHead = hd' }
    where hd      = sequentHead seq
          bdy     = sequentBody seq
          hdVars  = freeVars hd
          bdyVars = freeVars bdy
          hdFVars = hdVars \\ bdyVars
          allVars = hdVars `union` bdyVars

{- Applies addExistsPred on a theory of sequents. -}
addAllExistsPreds :: [Sequent] -> [Sequent]
addAllExistsPreds seqs = 
    State.evalState (run seqs) 0
    where run = foldM foldFunc []
          foldFunc res (Sequent b h) = do
            (h', seqs) <- addExistsPred h
            return $ res ++ (Sequent b h':seqs)

{- Uses addExistsPredHelper to relpace existential formulas, corresponding to 
   the head of a sequent, with fresh atomic formulas. As a consequence, new 
   sequents will be induced with the fresh atomic formula on left and 
   existential formula on right. The function returns the replaced head, from 
   the initial sequent, and the set of induced sequents. 
   The function requires a Counter to generate fresh relation symbols for the
   fresh atomic formulas. -}
addExistsPred :: Formula -> Counter (Formula, [Sequent])
addExistsPred fmla@(Or _ _) = addExistsPredHelper fmla
    -- Only apply the transformation if the head has disjunctions. That is, if
    -- the head is an existential formula, it is already in the form we want.
addExistsPred fmla          = return (fmla, [])

addExistsPredHelper :: Formula -> Counter (Formula, [Sequent])
addExistsPredHelper (Or f1 f2) = do
  (f1', seqs1) <- addExistsPredHelper f1
  (f2', seqs2) <- addExistsPredHelper f2
  return (Or f1' f2', seqs1 ++ seqs2)
addExistsPredHelper fmla@(Exists x f) = do
  let vs      = freeVars fmla
  sym         <- freshSymbol "@Exists"
  let atm     = R sym (map Var vs)
  let fmla'   = Atm atm
  (f', fSeqs) <- addExistsPredHelper f
  let seq     = Sequent fmla' (Exists x f')
  return (fmla', seq:fSeqs)
addExistsPredHelper f = return (f, []) -- Assumes normalized sequents

{- A helper for addElementPred, which adds @Element predicate to the right of 
   a sequent. The initial parameter of a list of variables is used to stack up
   all existentially quantified variables and add them in the most-inner level
   of the formula when the inductive steps are over. -}
addElementPredToRight :: Vars -> Formula -> Formula
addElementPredToRight _ (Or fmla1 fmla2) = -- go inside disjunction
    Or (addElementPredToRight [] fmla1) (addElementPredToRight [] fmla2)
    -- assuming normalized sequents
addElementPredToRight vs (And fmla1 fmla2) = 
    And (addElementPredToRight [] fmla1) (addElementPredToRight vs fmla2)
addElementPredToRight vs (Exists x fmla)  = -- add predicate for existentials
    Exists x (addElementPredToRight (x:vs) fmla)
addElementPredToRight vs fmla@(Atm atm@(R sym ts)) =
    let preds = (elementPred <$> ts') ++ ((elementPred.Var) <$> vs)
    in  foldr (\p f -> And (Atm p) f) fmla preds
                            -- This assumes flattened terms
    where ts' = filter (not.isVar) ts
addElementPredToRight _ fmla = fmla -- Note that we assume normalized sequents
                   

{- A helper for addElementPred. It constructs an @Element predicate for the 
   input term. -}
elementPred :: Term -> Atom
elementPred =  \x -> R "@Element" [x]

-- The following function is depricated since in the relational algebraic 
-- solution the theory is not extended.
-- {-| Updates a problem by adding new frames to its theory. It updates problem's 
--   symbol map accordingly.
-- -} 
-- extendProblem :: Problem -> [Frame] -> Problem
-- extendProblem (Problem oldFrames model queue last lastConst) 
--               frames =
--     Problem { problemFrames       = (union oldFrames newFrames) 
--             , problemModel        = model
--             , problemQueue        = queue
--             , problemLastID       = newLastID              
--             , problemLastConstant = lastConst}
--         -- REMARK: the two set of frames have to be unined; otherwise, the chase may
--         -- never terminate (as for thyphone1_2)
--     where newFrames = zipWith (\frame id -> frame {frameID = id})
--                       frames [(last + 1)..]
--           newLastID = last + (length newFrames)

{-| Selects a problem from a pool of problems based on the given scheduling type
  and returns the selected problem. The function changes the current state of 
  the pool of problems. If the pool is empty, the function returns Nothing. 
  Otherwise, returns the problem wrapped in Just.
-}
selectProblem :: ScheduleType -> ProbPool (Maybe Problem)
selectProblem _ =
    State.get >>= (\(fs, probs) ->
    case probs of
      []   -> return Nothing
      p:ps -> State.put (fs, ps) >>= (\_ -> 
              return $ Just p))

{-| Inserts a problem into the problem pool based on the given scheduling 
  type. -}
scheduleProblem :: Config -> Problem -> ProbPool ()
scheduleProblem cfg p = 
        State.get  >>= (\(fs, ps) ->
        State.put (fs, scheduleProblemHelper cfg p ps) >>= (\_ -> return ()))

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
processHead (Atm (R "=" _)) = error err_ChaseProblemOperations_EqTwoParam
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
processBody (Atm (R "=" _)) = error err_ChaseProblemOperations_EqTwoParam
processBody (Atm atm) = [Fct atm] -- atoms other than equality
processBody (Exists x p) = processBody p
processBody fmla = (error.show) fmla

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


-- testThy = ["Truth => (exists x . P(x))",
--            "P(x) =>  (exists x . P1(x) ) | (exists x . P2(x))",
--            "P1(x) =>  (exists x . P11(x) ) | (exists x . P12(x))",
--            "P11(x) =>  (exists x . P111(x) ) | (exists x . P112(x))",
--            "P111(x) =>  (exists x . P1111(x) ) | (exists x . P1112(x))"]

-- testThy = ["Truth => @Exists0()",
--            "@Exists0() => exists x. P(x)",

--            "P(x) => (@Exists1() | @Exists2())",
--            "@Exists1() => exists x. P1(x)",
--            "@Exists2() => exists x. P2(x)",

--            "P1(x) => (@Exists3() | @Exists4())",
--            "@Exists3() => exists x. P11(x)",
--            "@Exists4() => exists x. P12(x)",

--            "P11(x) => (@Exists5() | @Exists6())",
--            "@Exists5() => exists x. P111(x)",
--            "@Exists6() => exists x. P112(x)",

--            "P111(x) => (@Exists7() | @Exists8())",
--            "@Exists7() => exists x. P1111(x)",
--            "@Exists8() => exists x. P1112(x)"]
