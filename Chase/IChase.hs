{-| This module, contains our primary model-finding algorithm that will use 
  other modules.
-}
module Chase.IChase where

-- General Modules
import Data.List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Control.Monad.RWS.Lazy as RWS
import qualified Control.Monad.State.Lazy as State
import Control.Applicative
import Control.Monad

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities
import qualified Tools.Logger as Logger
import Tools.GeoUnification
import Tools.Config as Config
import Tools.Counter

-- Chase Modules
import Chase.Problem.BaseTypes
import Chase.Problem.Structures
import Chase.Problem.Operations
import Chase.Problem.Model(Model (..))
import qualified Chase.Problem.Model as Model
import Chase.Problem.RelAlg.RelAlg as RA

-- Other Modules
import Utils.Utils (allMaps, prodList, allSublists, allCombinations)
import Utils.Trace
import Debug.Trace


data ChaseStop = Fail | MaxSize

{-| Runs the chase for a given input theory, starting from an empty model.
-}
chase :: Config -> Theory -> [Model]
chase cfg thy = problemModel <$> runChase cfg Nothing frms initialProblem
    -- let probs = runChase cfg Nothing frms initialProblem
    -- in  [m | p <- probs, let m = problemModel p
    --     , let c = problemLastConstant p]
    where (frms, initialProblem) = buildProblem thy

{-| Like chase, but returns only the first model found.
-}
chase' :: Config -> Theory -> Maybe Model
chase' cfg thy = Maybe.listToMaybe $ chase cfg thy

{-| Runs the chase for an input theory atop an initial model.
-}
chaseWithModel :: Config -> Theory -> Model -> [Model]
chaseWithModel cfg thy mdl = 
    problemModel <$> runChase cfg (Just mdl) 
                 frms initialProblem
    where (frms, initialProblem) = buildProblem thy

{-| This is the main chase function, which tries to find the set of all the 
  models for a given geometric theory.
  Parameters:
  - The initial counter value for problem IDs
  - A Config instance
  - Possibly a starting (partial) model, mdl
  - A geometric theory, thy
-}
runChase :: Config -> Maybe Model -> FrameMap -> Problem -> [Problem]
runChase cfg mdl frms initialProblem = 
    let (probs, log) = State.evalState runCt cfg
    -- use the log information when needed
    in traceStringListIf (configDebug cfg) log
       -- traceStringList log
       probs
    where problem = case mdl of
                      Nothing -> initialProblem
                      Just m  -> initialProblem {problemModel = m}
          -- Create the initial problem
          runCt   = RWS.evalRWST runRWSt [] (frms, [])
          runRWSt = scheduleProblem problem >>= (\_ -> process) 

{-| Given an input problem, runs the chase and returns a set of final problems,
  which contain the models for the input problem.
-}
runChaseWithProblem :: Int -> Config -> FrameMap -> Problem -> [Problem]
runChaseWithProblem pid cfg frms problem =
    let (probs, log) = State.evalState runCt cfg
    in traceStringListIf (configDebug cfg) log
       probs
    where schedInfo = (problemScheduleInfo problem) {
                        problemSelectors = allFrameTypeSelectors }
          problem'  = problem {problemScheduleInfo = schedInfo }
          runCt     = RWS.evalRWST runRWSt [] (frms, [])
          runRWSt   = mapM scheduleProblem [problem'] >>= (\_ -> process)
          -- schedule the problem, then process it


{- Combines the outputs of stepProblets -}
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
      cfg              <- liftConfig State.get 
      prob             <- selectProblem -- pick a problem
      (fMap, allProbs) <- RWS.get                          
      -- Logger.logUnder (length allProbs) "# of branches"                          
      -- Logger.logIf (Maybe.isJust prob) ((problemModel.Maybe.fromJust) prob)
      case prob of
        Nothing -> return [] -- no more problems
        Just p  -> cascadeStep fMap p

{- Runs chase steps on a problem according to problemScheduleInfo in the 
   problem. The function runs the frames that satisfy the first set of frame 
   selectors. If the selected frames create new problems, the problems will be 
   scheduled and processed using "process". Otherwise, if the selected frames 
   lead to failure (Left xx), then the current frame will be ignored and 
   the control flow goes back to process. Finally, if the selected list of 
   frames do not produce new problems (i.e., they do not add new facts to the 
   model) cascadeStep will be called on the next set of frame selectors in 
   problemScheduleInfo.-}
cascadeStep :: FrameMap -> Problem -> ProbPool [Problem]
cascadeStep fMap prob 
    | null selectors = -- Running out of selectors: model satisfies the theory
        do { rest <- process             
           ; return (prob:rest) }
    | otherwise = 
        do { let (sl:sls) = selectors
           ; let age'  = if null sls then age + 1 else age
                 -- Increase age if a big step is applied, i.e., sls is empty.
           ; let schedInfo' = schedInfo { problemSelectors  = sls
                                        , problemBigStepAge = age'}
           ; let prob' = prob {problemScheduleInfo = schedInfo'}
           ; newProbs <- chaseStep fMap sl prob'
           ; case newProbs of
               Left _   -> process
               Right [] -> cascadeStep fMap prob'
                 -- Try another set of sequents if the sequents in the current 
                 -- set didn't get fired.
               Right ps -> do
                 mapM scheduleProblem ps -- Schedule new problems
                 process }               -- Process the pool
    where schedInfo@ScheduleInfo { problemSelectors  = selectors
                                 , problemBigStepAge = age } = 
                                 problemScheduleInfo prob

{- Problet is a minimal version of Problem, which represents a problem in a 
   chase step. Problet encapsulates exactly the amount of information that is 
   required to reconstruct problems by the layer above, i.e., cascadeSteps. -}
data Problet = Problet { probletModel        :: Model,
                         probletQueue        :: Tables,
                         probletLastConstant :: Int }

{- Builds a Problem from a Problet. Because a Problem contains more information,
   the function requires additional information other than the input Problet. -}
fromProblet :: [ID] -> [Tables] -> ScheduleInfo ->  Problet -> Problem
fromProblet frms initialQueue schedInfo (Problet m q c) =
    Problem { problemFrames       = frms
            , problemModel        = m
            , problemQueue        = initialQueue ++ [q]
            , problemScheduleInfo = schedInfo
            , problemLastConstant = c}

{- Applies a chase step to the input problem. -}
chaseStep :: FrameMap -> [FrameTypeSelector] -> Problem -> 
             ProbPool (Either ChaseStop [Problem])
chaseStep fMap sl p@(Problem frmIDs model queues schedInfo lastConst) = do
  cfg <- liftConfig Config.get
  return $ do
      let (q:qs) = if null queues then [emptyTables] else queues
      let bound = configBound cfg
      when (Maybe.isJust bound &&
                 length (Model.modelDomain model) > Maybe.fromJust bound) 
               (Left MaxSize)
               -- When a bound is given, if the model size is greater than the 
               -- bounds then fail.
      let schedInfo' = schedInfo { problemSelectors  = allFrameTypeSelectors }
      let problet    = Problet model q lastConst
      (frms, probs) <- stepProblets cfg fMap frmIDs sl problet
      return $ map (\prob -> fromProblet frms qs schedInfo' prob) probs


{- Constructs a set of new Problets for an input given problet. The function
   requires a Config instance, frames of the theory (FrameMap), a list of IDs 
   for the current state of the frame queue in the original problem, and a set 
   of selectors to choose frames in addition to the input Problet. -}
stepProblets :: Config -> FrameMap -> [ID] -> [FrameTypeSelector] -> Problet 
             -> Either ChaseStop ([ID], [Problet])
stepProblets _ _ [] _ _ = Right ([], [])
stepProblets cfg fMap frmIDs sl 
             p@(Problet model queue counter) = do
      let (frm, fs)  = selectFrame fMap frmIDs sl -- choose a frame
      if Maybe.isJust frm
        then do
          let id = Maybe.fromJust frm
          let f  = Maybe.fromJust $ Map.lookup id fMap
          curr         <- newProbletsForFrame cfg f p
          (fs', oth)   <- stepProblets cfg fMap fs sl p
          if   null curr
            then return (scheduleFrame fMap id fs', oth)
            else return (scheduleFrame fMap id fs , curr)
        else return ([], [])

newProbletsForFrame :: Config -> Frame -> Problet -> Either ChaseStop [Problet]
newProbletsForFrame cfg frame prob = do
  (os, c, p, skTerm)   <- newFacts cfg frame prob
  let model     = probletModel prob
  let depth     = configSkolemDepth cfg
  let mapFunc o = (\(m', ts', c') -> Problet m' ts' c') $
                  Model.add model c o (Maybe.fromJust p) (skTerm, depth)
  return $ map mapFunc os


frameSkolemTerm frm sub = let Fct (R s ts) = liftTerm (liftSub sub) $ head (frameBody frm)
                          in  Fn s ts

                      
newFacts :: Config -> Frame -> Problet -> 
            Either ChaseStop ([[Obs]], Int, Maybe Prov, Maybe SkolemTerm)
newFacts cfg frame (Problet model queue counter)
    | null subs = Right ([], counter, Nothing, skTerm)
    | (null.frameHead) frame = Left Fail
    | otherwise = let prov = ChaseProv provTag (frameID frame) theSub
                  in  Right $ glue (deduceForFrame cfg counter model heads) 
                                   (Just prov)
    where subs      = matchFrame model queue (frameRelInfo frame)
                      (configIncremental cfg)
          theSub    = head subs -- Batch processing is off
          -- lifted    = liftFrame model theSub frame
          -- heads     = frameHead lifted
          -- vars      = frameVars lifted
          heads     = liftTerm (liftSub theSub) (frameHead frame)
          vars      = []
          provTag   = (provInfoLastTag.Model.modelProvInfo) model
                      -- Construct the provenance information for the new facts
                      -- being deduced.
          glue      = \(x, y) z -> (x, y, z, skTerm)
          -- For now just use the first sub and drop the rest!
          -- Also, let's just lift the body for now!
          -- Salman: Separate provenance computation from the main computation.
          domain    = Model.modelDomain model 
          skTerm    = if   hasFrameType frame [ftExistential]
                      then Just (frameSkolemTerm frame theSub)
                      else Nothing

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
deduceForFrame :: Config -> Int -> Model -> [[Obs]] -> ([[Obs]], Int)
deduceForFrame cfg counter model hs =
    foldr (\h (rest, c) -> 
           let (result, newCounter) = deduce cfg c model h
           in  (result ++ rest, newCounter)) ([], counter) hs
                        
{- Deduces new facts for a list of conjuncted Obs and a model. This method 
   is called on right of sequents whose body is empty. Vars is a set of 
   universally quantified variables in the observations while the other 
   variables are assumed to be existentially quantified.

   The inputs for this function consist of (1) an old counter to keep track 
   of the number of elements added by the chase, (2) a model, and (3) the 
   input observations.
-}
deduce :: Config -> Int -> Model -> [Obs] -> ([[Obs]], Int)
deduce cfg counter model hs = deduceHelper cfg counter model hs
        -- Call a helper function to deduce new facts.

{- This is a helper function for deduce. It infers a list of observations
   to add to the input model in which the observations are not true.
-}
-- deduceHelper _ counter _ [] = ([[]], counter)
-- deduceHelper cfg counter model allObs@(obs:rest)
--     | null existVars = 
--         let (restObs, restCounter) = deduceHelper cfg counter model rest 
--         in  ((obs:) <$> restObs, restCounter)
--             -- The observation is a closed atom. Return it together with
--             -- the observations corresponding to the rest of the facts.
--     | otherwise      =
--         let freshConstants = map makeFreshConstant [(counter + 1)..]
--             -- existSubs      = Map.fromList $ zip existVars freshConstants
--             existSubs      = allExistsSubs cfg counter existVars 
--                              (Model.modelDomain model)
--             liftedRest     = (\s -> map ((liftTerm.liftSub) s) allObs) <$> existSubs
--             newCounter     = counter + length existVars
--             restDeduced    = deduceHelper cfg newCounter model <$> liftedRest
--             (fcts, cs)     = unzip restDeduced
--         in  (concat fcts, head cs) 
--             -- In this case, all of the free variables in the observation 
--             -- are existentials: instantiate variables with fresh constants; 
--             -- then, apply deduce on the instantiated observations.
--     where existVars = freeVars obs -- existentially quantified vars in obs

deduceHelper _ counter _ [] = ([[]], counter)
deduceHelper cfg counter model allObs@(obs:rest)
    | null existVars = 
        let (restObs, restCounter) = deduceHelper cfg counter model rest 
        in  ((obs:) <$> restObs, restCounter)
            -- The observation is a closed atom. Return it together with
            -- the observations corresponding to the rest of the facts.
    | otherwise      =
        let freshConstants = map makeFreshConstant [(counter + 1)..]
            -- existSubs      = Map.fromList $ zip existVars freshConstants
            existSubs      = allExistsSubs cfg counter existVars 
                             (Model.modelDomain model)
            liftedRest     = (\s -> map ((liftTerm.liftSub) s) allObs) <$> existSubs
            newCounter     = counter + length existVars
            restDeduced    = deduceHelper cfg newCounter model <$> liftedRest
            (fcts, cs)     = unzip restDeduced
        in  (concat fcts, head cs) 
            -- In this case, all of the free variables in the observation 
            -- are existentials: instantiate variables with fresh constants; 
            -- then, apply deduce on the instantiated observations.
    where existVars = freeVars obs -- existentially quantified vars in obs


allExistsSubs :: Config -> Int -> Vars -> [Elem] -> [Sub]
allExistsSubs cfg counter vars dom = 
    let subLists = zip vars <$> lists
    in  Map.fromList <$> subLists
    where lists  = subValues counter vars

{- For various scheduling strategies based on collapse and extend, generates a
   list of values to which a variable can be assigned. -}
-- subValues :: Int -> Vars -> [Elem] 
subValues counter vars = lists $ pure <$> freshCs 
    where freshCs  = makeFreshConstant <$> 
                     [(counter + 1)..(counter + varsNum)]
          varsNum  = length vars
          lists    = foldr (\opts res -> 
                            let opts' = pure <$> opts
                            in  prodList opts' res) []

-- arrangeLists :: [Term] -> [[Term]] -> [[Term]]
-- arrangeLists fresh lists = pre ++ post
--     where (post, pre) = partition (\l -> any (`elem` l) fresh) lists


{- Lifts a frame with a substitution and returns a new frame. It also updates
   the list of free variables of the frame accordingly.
-}
liftFrame :: Model -> Sub -> Frame -> Frame
liftFrame model sub frame = 
    let Frame id b h v o ft = liftTerm (liftSub sub) frame
    in  Frame id b h (v \\ Map.keys sub) o ft

{- Helper Functions -}
{- make a new constant (special element starting with "a") -}
makeFreshConstant :: Int -> Term
makeFreshConstant counter = Fn ("a@" ++ (show counter)) []


-- Run the chase for local tests:
debugConf = defaultConfig { configDebug       = False
                          , configBound       = Nothing
                          , configSchedule    = SchedBFS
                          , configIsoElim     = False 
                          , configSkolemDepth = -1 }

doChase  thy = chase  debugConf $ map parseSequent thy
doChase' thy = chase' debugConf $ map parseSequent thy


test cfg thy = 
    runChase cfg Nothing frms initialProblem
    where (frms, initialProblem) = buildProblem thy

doTest thy = test debugConf $ map parseSequent thy