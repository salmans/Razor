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
import qualified Chase.Problem.RelAlg.Operations as OP -- remove this
import Chase.Problem.RelAlg.RelAlg as RA

-- Other Modules
import Utils.Utils (allMaps, prodList, allSublists, 
                    allCombinations)
import Utils.Trace
import Debug.Trace

{-| Runs the chase for a given input theory, starting from an empty model.
-}
chase :: Config -> Theory -> [Model]
chase cfg thy = problemModel <$> runChase cfg Nothing thy

{-| Like chase, but returns only the first model found.
-}
chase' :: Config -> Theory -> Maybe Model
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
          run            = scheduleProblem (configSchedule cfg) problem >>= 
                           (\_ -> process)          
          -- schedule the problem, then process it

{-| Given an input problem, runs the chase and returns a set of final problems,
  which contain the models for the input problem.
-}
runChaseWithProblem :: Config -> Problem -> [Problem]
runChaseWithProblem cfg problem =
    let (probs, log) = State.evalState config cfg
    in traceList log
       probs
    where run    =  mapM (scheduleProblem (configSchedule cfg)) [problem] >>= 
                    (\_ -> process)
          config = RWS.evalRWST run [] []
          -- schedule the problem, then process it


{- Combines the outputs of newModels -}
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



{- For a frame whose body is empty, returns a pair containing a list of list of 
   obs, which are deduced from the right of the frame. Every item in an inner 
   list, corresponds to the deduced terms for a disjunct on right; thus, the 
   outer list contains a list of all the Obs deduced from all the disjuncts 
   on right. The list of deduced facts is empty if any of the disjuncts in the 
   head is true in the current model.
   It also returns a new value for problemLastConstant of the problem to which 
   the frame belongs. If the frame's body is not empty, it simply returns an 
   empty list and the input problemLastConstant.
   The parameters are (1) problemLastConstant of the problem to which the frame 
   belongs to, (2) the problemModel of the problem, and (3) finally the frame.
-}
-- deduceForFrame :: Int -> Model -> Frame -> ([[Obs]], Int)
-- deduceForFrame counter model frame@(Frame _ body head vars _)
--     -- Salman: this may break!
--    | null body && not (any (holds model vars) head) = 
--        deduceForFrameHelper counter model vars head
--    | otherwise = ([], counter)

{- This is a helper for deduceForFrame. It handles the recursive calls over the
   disjuncts on the right of the frame.
-}
deduceForFrameHelper :: Int -> Model -> Vars -> [[Obs]] -> ([[Obs]], Int)
deduceForFrameHelper counter model vars hs =
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
    | null obsVars = 
        let (restObs, restCounter) = deduceHelper counter model vars rest 
        in (obs: restObs, restCounter)
            -- The observation is a closed atom. Return it together with
            -- the observations corresponding to the rest of the facts.
    | null domain && (not.null) univVars = ([], counter) 
    | otherwise   =
        let freshConstants = map makeFreshConstant [(counter + 1)..]
            existSubs      = zip existVars freshConstants
            univSubs       = [zip univVars elems | elems <- permutations domain]
            allSubs        = Map.fromList.((++) existSubs) <$> univSubs
            liftedRest     = (\sub -> (liftTerm.lift) sub <$> allObs) 
                             <$> allSubs
            newCounter     = counter + length existVars
        in foldr (\lifted (obs, _) -> 
                  let (obs', c') = deduceHelper newCounter model vars lifted
                  in  (obs ++ obs', c')) ([], counter) liftedRest
    where obsVars   = freeVars obs
          existVars = obsVars \\ vars -- existential variables in obs
          univVars  = obsVars `intersect` vars
          domain    = Model.modelDomain model -- for instantiating universals

{- Lifts a frame with a substitution and returns a new frame. It also updates
   the list of free variables of the frame accordingly.
-}
liftFrame :: Model -> Sub -> Frame -> Frame
liftFrame model sub frame = 
    let Frame id b h v o ft p = liftTerm (lift sub) frame
    in  Frame id b h (v \\ Map.keys sub) o ft p

{- Helper Functions -}
{- make a new constant (special element starting with "a") -}
makeFreshConstant :: Int -> Term
makeFreshConstant counter = Fn ("a@" ++ (show counter)) []
--Elm ("a" ++ show counter)

{- Processes the problems in the pool. Applies a chase step to the problem 
   chosen by selectProblem.It schedules the problems created by this chase
   step. -}
process :: ProbPool [Problem]
process = do
      cfg  <- State.lift State.get
      prob <- selectProblem (configSchedule cfg) -- pick a problem
      allPs <- RWS.get
      Logger.logUnder (length allPs) "# of branches"
      Logger.logIf (Maybe.isJust prob) ((problemModel.Maybe.fromJust) prob)
      case prob of
        Nothing -> return [] -- no more problems
        Just p  -> 
            do
              let newProbs = newProblems cfg p
              case newProbs of
                Left CSTFail -> process
                Left CSTProcessed -> do -- continue untill all processed
                  rest <- process
                  return (p:rest)
                Right [] -> do
                  scheduleProblem (configSchedule cfg) p
                  process
                Right ps -> do 
                  mapM (scheduleProblem (configSchedule cfg)) ps
                  process
              

{- Applies a chase step to the input problem. -}
newProblems :: Config -> Problem -> Either ChaseStopType [Problem]
newProblems cfg problem@(Problem frames model [] lastID lastConst) =
    case newModels cfg model emptyTables frames lastConst of
      Left x                  -> Left x
      Right (_ ,     []     ) -> Right []
      Right (frames', models) -> 
          Right $ map (\(m, q, c) -> 
                      Problem { problemFrames       = frames'
                              , problemModel        = m
                              , problemQueue        = [q]
                              , problemLastID       = lastID
                              , problemLastConstant = c}) models
newProblems cfg problem@(Problem frames model (queue:queues) lastID lastConst) =
    case newModels cfg model queue frames lastConst of
      Left x                  -> Left x
      Right (_    , []      ) -> Right []
      Right (frames', models) -> 
          Right $ map (\(m, q, c) -> 
                       Problem { problemFrames       = frames'
                               , problemModel        = m
                               , problemQueue        = queues ++ [q]
                               , problemLastID       = lastID
                               , problemLastConstant = c}) models

data ChaseStopType = CSTProcessed 
                   | CSTFail
  deriving Show
-- Helpers for newProblems:
-- Salman: add comments!
newModels :: Config -> Model -> Tables -> [Frame] -> Int ->
             Either ChaseStopType ([Frame], [(Model, Tables, Int)])
newModels _ _ _ [] _ = Right ([], [])
-- Salman: redo this:
newModels cfg model queue frames counter = do
  let (frame, fs)  = selectFrame frames
  if  Maybe.isJust frame -- No more frames
      then do        
          let f           = Maybe.fromJust frame
          let incremental = configIncremental cfg
          -- let batch        = configBatch cfg
          curr            <- newModelsForFrame model queue f counter incremental
                             -- done processing this frame unless it gets reset

          -- A hook to batch processing:
          -- if   batch  -- if batch processing is enabled
          -- then return (fs', curr <|> oth)
          -- else 

          -- Since we are not doing batch processing now, after getting the new
          -- facts for the current sequent, we immediately return. That is, a 
          -- call to newModels returns an updated model for the first set of 
          -- facts that are constructed by the first applicable sequent. 
          -- In order to make sure that there is no sequents is unprocessed, 
          -- the function has to be called until no unprocessed sequent is left.
          if   null curr
          then do
            let scheduled = scheduleFrame (f {frameProcessed = True }) fs
            newModels cfg model queue scheduled counter
          else return (scheduleFrame f (resetFrame <$> fs), curr)
      else Left CSTProcessed

resetFrame :: Frame -> Frame
resetFrame frame           
    | frame `hasFrameType` [(not.ftUniversal), ftInitial] = frame
    | otherwise = frame { frameProcessed = False }


newModelsForFrame :: Model -> Tables -> Frame -> Int -> Bool
                     -> Either ChaseStopType [(Model, Tables, Int)]
newModelsForFrame model queue frame counter incremental = 
    case newFacts counter model queue frame incremental of
      Nothing         -> Left CSTFail
      Just ([], _, _) -> Right []
      Just (os, c, p) -> Right $ (\o -> 
                                  Model.add model c o (Maybe.fromJust p)) <$> os
                      
newFacts :: Int -> Model -> Tables -> Frame -> Bool -> 
            Maybe ([[Obs]], Int, Maybe Prov)
-- Salman: this is too complicated. Is it possible to make the code simpler?
newFacts counter model queue frame incremental 
    | null subs = Just ([], counter, Nothing)
    | (null.frameHead) frame = Nothing
    -- The next two cases are artificial but they are required to deal with 
    -- free variables on right when the model is empty. In cases like this, we
    -- want to make sure that no substitutions are produced for the branches 
    -- with free variables on right.
    -- First, if the model is empty and every branch on right has free 
    -- variables, immediately return no substitutions.
    | null domain && null nonFreeHs =
        Just ([], counter, Nothing)
    -- Otherwise, if the model is empty but there are some branches on right
    -- that don't have free variables, just apply deduce on those branches.
    | null domain =
        Just $ glue (deduceForFrameHelper counter model vars nonFreeHs)
                    (Just prov)
    -- And of course, treat the sequent normally if the model is not empty.
    | otherwise = Just $ 
                  glue (deduceForFrameHelper counter model vars heads)
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
          nonFreeHs = filter (\h -> null ((freeVars h) `intersect` vars)) heads
          glue      = \(x, y) z -> (x, y, z)
          -- For now just use the first sub and drop the rest!
          -- Also, let's just lift the body for now!
          -- Salman: Separate provenance computation from the main computation.
          domain    = Model.modelDomain model 

-- Run the chase for local tests:
debugConf = defaultConfig { configDebug = False }
doChase thy  = chase  debugConf $ map parseSequent thy
doChase' thy = chase' debugConf $ map parseSequent thy