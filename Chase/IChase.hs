{-| This module, contains our primary model-finding algorithm that will use 
  other modules.
-}
module Chase.IChase where

-- General Modules
import Data.List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.State as State
import Control.Applicative
import Control.Monad

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities
import qualified Tools.Logger as Logger
import Tools.GeoUnification
import Tools.Config

-- Chase Modules
import Chase.Problem.BaseTypes
import Chase.Problem.Provenance
import Chase.Problem.Observation
import Chase.Problem.Structures
import Chase.Problem.Operations
import Chase.Problem.Model(Model (..))
import qualified Chase.Problem.Model as Model
import Chase.Problem.RelAlg.RelAlg as RA

-- Other Modules
import Utils.Utils (allMaps, prodList, allSublists, 
                    allCombinations)
import Utils.Trace
import Debug.Trace


data ChaseStop = ChaseFail | ChaseAged

{-| Runs the chase for a given input theory, starting from an empty model.
-}
chase :: Config -> Theory -> [Problem]
chase cfg thy = runChase cfg Nothing thy

{-| Like chase, but returns only the first model found.
-}
chase' :: Config -> Theory -> Maybe Problem
chase' cfg thy = Maybe.listToMaybe $ chase cfg thy

{-| Runs the chase for a set of input theories over an input (partial) model. -}
chaseWithModel :: Model -> Theory -> [Model]
chaseWithModel mdl thy = problemModel <$> runChase defaultConfig (Just mdl) thy 

{-| This is the main chase function, which tries to find the set of all the 
  models for a given geometric theory.
  Parameters:
  - A geometric theory, thy
  - Possibly a starting (partial) model, mdl
-}
runChase :: Config -> Maybe Model -> Theory -> [Problem]
runChase cfg mdl thy = 
    let (probs, log) = State.evalState config cfg
    -- use the log information when needed
    in traceStringListIf (configDebug cfg) log
       probs
    where initialProblem = buildProblem (relConvert thy) 
          -- Create the initial problem (get rid of function symbols)
          problem        = case mdl of
                             Nothing -> initialProblem
                             Just m  -> initialProblem {problemModel = m}
          config         = RWS.evalRWST run [] []
          run            = scheduleProblem cfg problem >>= (\_ -> process)          
          -- schedule the problem, then process it

{-| Given an input problem, runs the chase and returns a set of final problems,
  which contain the models for the input problem.
-}
runChaseWithProblem :: Config -> Problem -> [Problem]
runChaseWithProblem cfg problem =
    let (probs, log) = State.evalState config cfg
    in traceList log
       probs
    where run    =  mapM (scheduleProblem cfg) [problem] >>= (\_ -> process)
          config = RWS.evalRWST run [] []
          -- schedule the problem, then process it


{- Combines the outputs of stepModels -}
-- combine :: [(Model, Tables, Int)] -> [(Model, Tables, Int)]
--            -> [(Model, Tables, Int)]
-- combine new old = 
--     if null old
--     then new
--     else [(mergeModels nm om, mergeSets nq oq, max nc oc)
--           | (nm, nq, nc) <- new
--          , (om, oq, oc) <- old]
--     where mergeModels (Model tbls1 provs1) (Model tbls2 provs2) = 
--               Model (mergeSets tbls1 tbls2) (Map.unionWith (++) provs1 provs2)


{- Processes the problems in the pool by applying chase steps to the problem 
   chosen by selectProblem.It schedules the problems created by this chase
   step. -}
process :: ProbPool [Problem]
process = do
      cfg      <- State.lift State.get
      prob     <- selectProblem (configSchedule cfg) -- pick a problem
      allProbs <- RWS.get
      Logger.logUnder (length allProbs) "# of branches"
      Logger.logIf (Maybe.isJust prob) ((problemModel.Maybe.fromJust) prob)
      case prob of
        Nothing -> return [] -- no more problems
        Just p  -> cascadeStep cfg p

{- Runs chase steps on a problem according to problemScheduleInfo in the 
   problem. The function runs the frames that satisfy the first list of frame 
   type selectors. If the selected frames create new problems, the problems 
   will be scheduled and processed using process function. Otherwise, if the 
   selected frames lead to Nothing, then the current frame will be ignored and 
   the control flow goes back to process. Finally, if the selected list of 
   frames do not produce new problems (i.e., they do not add new facts to the 
   model) cascadeStep will be called on the next set of frame selectors in 
   problemScheduleInfo.-}
cascadeStep :: Config -> Problem -> ProbPool [Problem]
cascadeStep cfg prob 
    | null selectors = 
        do { rest <- process
           ; return (prob:rest) }
    | otherwise = 
        do { let (tps:restTps) = selectors
           ; newProbs <- chaseStep cfg tps prob
           ; let age' = if null restTps then age + 1 else age
             -- add to age if a big step is applied, i.e., restTps is empty.
           ; let schedInfo' = ScheduleInfo { problemFrameSelector = restTps
                                           , problemBigStepAge    = age'}
           ; case newProbs of
               Left _   -> process
               Right [] -> 
                   cascadeStep cfg prob { problemScheduleInfo = schedInfo' }
               Right ps -> do
                 mapM (scheduleProblem cfg) ps
                 process }
    where ScheduleInfo { problemFrameSelector = selectors
                       , problemBigStepAge    = age } = 
                                                  problemScheduleInfo prob

{- Applies a chase step to the input problem. A big step can potentially 
   increase the size of the model, i.e., it processes sequents with 
   existentials. -}
chaseStep :: Config -> [FrameTypeSelector] -> Problem -> 
             ProbPool (Either ChaseStop [Problem])
chaseStep cfg frmTps problem@(Problem frames model [] schedInfo lastConst) =
    return $ do 
      let schedInfo' = 
              schedInfo { problemFrameSelector = allFrameTypeSelectors }
              -- reset frame selectors
      (frms, mdls) <- stepModels cfg frmTps model emptyTables frames lastConst
      return $ map (\(m, q, c) -> Problem { problemFrames       = frms
                                          , problemModel        = m
                                          , problemQueue        = [q]
                                          , problemScheduleInfo = schedInfo'
                                          , problemLastConstant = c }) mdls
chaseStep cfg frmTps 
          problem@(Problem frames model (queue:queues) schedInfo lastConst) =
    return $ do 
      let schedInfo' = 
             schedInfo { problemFrameSelector = allFrameTypeSelectors }
             -- reset frame selectors
      (frms, mdls) <- stepModels cfg frmTps model queue frames lastConst
      return $ map (\(m, q, c) -> Problem { problemFrames       = frms
                                          , problemModel        = m
                                          , problemQueue        = queues ++ [q]
                                          , problemScheduleInfo = schedInfo'
                                          , problemLastConstant = c }) mdls

-- Salman: add comments!
stepModels :: Config -> [FrameTypeSelector] -> Model -> Tables -> [Frame] -> 
              Int -> Either ChaseStop ([Frame], [(Model, Tables, Int)])
stepModels _ _ _ _ [] _ = Right ([], [])
-- Salman: redo this:
stepModels cfg frmTps model queue frames counter = do
      let (frm, fs)  = selectFrame frames frmTps
      if Maybe.isJust frm
        then do
          let f = Maybe.fromJust frm                  
          curr         <- newModelsForFrame model queue f counter 
                              (configIncremental cfg)
          (fs', oth)   <- stepModels cfg frmTps model queue fs counter
          if   null curr
            then return (scheduleFrame f fs', oth)
            else return (scheduleFrame f fs , curr)
        else return ([], [])

newModelsForFrame :: Model -> Tables -> Frame -> Int -> Bool
                     -> Either ChaseStop [(Model, Tables, Int)]
newModelsForFrame model queue frame counter incremental = do
  (os, c, p) <- newFacts counter model queue frame incremental
  return $ map (\o -> Model.add model c o (Maybe.fromJust p)) os

                      
newFacts :: Int -> Model -> Tables -> Frame -> Bool -> 
            Either ChaseStop ([[Obs]], Int, Maybe Prov)
-- Salman: this is too complicated. Is it possible to make the code simpler?
newFacts counter model queue frame incremental 
    | null subs = Right ([], counter, Nothing)
    | (null.frameHead) frame = Left ChaseFail
    | otherwise = Right $ 
                  glue (deduceForFrame counter model vars heads)
                       (Just prov)
    where subs      = matchFrame (modelTables model) queue 
                               (frameRelInfo frame) incremental
          theSub    = head subs
          lifted    = liftFrame model theSub frame
          heads     = frameHead lifted
          vars      = frameVars lifted
          prov      = ChaseProv provTag (frameID frame) theSub
          provTag   = (provInfoLastTag.Model.modelProvInfo) model
                      -- Construct the provenance information for the new facts
                      -- being deduced.
          glue      = \(x, y) z -> (x, y, z)
          -- For now just use the first sub and drop the rest!
          -- Also, let's just lift the body for now!
          -- Salman: Separate provenance computation from the main computation.
          domain    = Model.modelDomain model 

{- For a frame (assumed to be triggered) returns a pair containing a list of 
   list of obs, which are deduced from the right of the frame. Every item in an 
   inner list, corresponds to the deduced terms for a disjunct on right; thus, 
   the outer list contains a list of all the Obs deduced from all the disjuncts 
   on right. The list of deduced facts is empty if any of the disjuncts in the 
   head is true in the current model.
   It also returns a new value for problemLastConstant of the problem to which 
   the frame belongs. If the frame's body is not empty, it simply returns an 
   empty list and the input problemLastConstant.
   The parameters are (1) problemLastConstant of the problem to which the frame 
   belongs to, (2) the problemModel of the problem, and (3) finally the frame.
-}
deduceForFrame :: Int -> Model -> Vars -> [[Obs]] -> ([[Obs]], Int)
deduceForFrame counter model vars hs =
    foldr (\h (rest, c) -> 
           let (result, newCounter) = deduce c model vars h
           in  (result:rest, newCounter)) ([], counter) hs
                        
{- Deduces new facts for a list of conjuncted Obs and a model. This method 
   is called on right of sequents whose body is empty. Vars is a set of 
   universally quantified variables in the observations while the other 
   variables are assumed to be existentially quantified.

   The inputs for this function consist of (1) an old counter to keep track 
   of the number of elements added by the chase, (2) a model, and (3) the 
   input observations.
-}
deduce :: Int -> Model -> Vars -> [Obs] -> ([Obs], Int)
deduce counter model vars hs = deduceHelper counter model vars hs
        -- Call a helper function to deduce new facts.

{- This is a helper function for deduce. It infers a list of observations
   to add to the input model in which the observations are not true.
-}
deduceHelper counter _ _ [] = ([], counter)
deduceHelper counter model vars allObs@(obs:rest)
    | null existVars = 
        let (restObs, restCounter) = deduceHelper counter model vars rest 
        in (obs: restObs, restCounter)
            -- The observation is a closed atom. Return it together with
            -- the observations corresponding to the rest of the facts.
    | null $ existVars `intersect` vars =
        let freshConstants = map makeFreshConstant [(counter + 1)..]
            existSubs      = Map.fromList $ zip existVars freshConstants
            liftedRest     = map ((liftTerm.lift) existSubs) allObs
            newCounter     = counter + length existVars
        in deduceHelper newCounter model vars liftedRest
            -- In this case, all of the free variables in the observation 
            -- are existentials: instantiate variables with fresh constants; 
            -- then, apply deduce on the instantiated observations.
    | otherwise = error $ "Chase.Chase.deduceHelper: the input " ++
                   "formula cannot have free variables."
            -- There is a universally quantified variable in the obs. We don't
            -- support universally quantified on right when they are not defined
            -- on left of a sequent. So, there is something wrong!
    where existVars = freeVars obs -- existentially quantified vars in obs

{- Lifts a frame with a substitution and returns a new frame. It also updates
   the list of free variables of the frame accordingly.
-}
liftFrame :: Model -> Sub -> Frame -> Frame
liftFrame model sub frame = 
    let Frame id b h v o ft = liftTerm (lift sub) frame
    in  Frame id b h (v \\ Map.keys sub) o ft

{- Helper Functions -}
{- make a new constant (special element starting with "a") -}
makeFreshConstant :: Int -> Term
makeFreshConstant counter = Fn ("a@" ++ (show counter)) []


-- Run the chase for local tests:
debugConf = defaultConfig { configDebug = False }
doChase thy  = chase  debugConf $ map parseSequent thy
doChase' thy = chase' debugConf $ map parseSequent thy