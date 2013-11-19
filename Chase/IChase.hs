{-

   This module, contains our primary model-finding algorithm
   that will use other modules.
-}
module Chase.IChase where

-- General Modules
import Data.List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Control.Monad.State as State
import qualified Control.Monad.Writer as Writer
import Control.Applicative
import Control.Monad

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities
import Tools.GeoUnification

-- Chase Modules
import Chase.Problem.BaseTypes
import Chase.Problem.Observation
import Chase.Problem.Structures
import Chase.Problem.Operations
import Chase.Problem.Model(Model (..))
import qualified Chase.Problem.Model as Model
import qualified Chase.Problem.RelAlg.Operations as OP -- remove this
import Chase.Problem.RelAlg.RelAlg as RA

-- Other Modules
import Utils.Utils (allMaps, prodList, allSublists, allCombinations)
import Debug.Trace
import Tools.Logger

import qualified Codec.TPTP as TPTP

{-| Runs the chase for a given input theory, starting from an empty model.
-}
chase :: Theory -> [Model]
chase thy = problemModel <$> runChase Nothing thy

{-| Like chase, but returns only the first model found.
-}
chase' :: Theory -> Maybe Model
chase' thy = Maybe.listToMaybe $ chase thy

{-| Runs the chase for a set of input theories over an input (partial) model. -}
chaseWithModel :: Model -> Theory -> [Model]
chaseWithModel mdl thy = problemModel <$> runChase (Just mdl) thy 

{-| This is the main chase function, which tries to find the set of all the 
  models for a given geometric theory.
  Parameters:
  - A geometric theory, thy
  - Possibly a starting (partial) model, mdl
-}
runChase :: Maybe Model -> Theory -> [Problem]
runChase mdl thy =
    let ((probs, _), log) = Writer.runWriter writer in
    -- use the log information when needed
    -- (trace.show) log
    probs
    where initialProblem    = buildProblem (relConvert thy) 
          problem           = case mdl of
                                Nothing -> initialProblem
                                Just m  -> initialProblem {problemModel = m}
          problems          = Maybe.fromMaybe [] $ initiateProblem problem
          -- Create the initial problem (get rid of function symbols)
          writer            = State.runStateT run [] 
                              -- the wrapper Writer monad for logging
          run               =  (mapM scheduleProblem problems) >>= (\_ -> 
                              process) -- schedule the problem, then process it


{- Processes the frames with Truth on their lefts and initiates the queue of
   the problem accordingly. It removes the processed frames from the problem. -}
initiateProblem :: Problem -> Maybe [Problem]
initiateProblem problem@(Problem frames model [] lastID lastConst) =
    case newModels' model emptyTables initiatingFrames lastConst of
      Nothing     -> Nothing
      Just []     -> Just $ [Problem { problemFrames       = otherFrames
                                     , problemModel        = model
                                     , problemQueue        = []
                                     , problemLastID       = lastID
                                     , problemLastConstant = lastConst}]
      Just models -> Just $ map (\(m, q, c) -> 
                                 Problem { problemFrames       = otherFrames
                                         , problemModel        = m
                                         , problemQueue        = [q]
                                         , problemLastID       = lastID
                                         , problemLastConstant = c}) models
    where (initiatingFrames, otherFrames) = partition (null.frameBody) frames
initiateProblem _ = error $ "Chase.IChase.initateProblem: the problem is has "
                    ++ "already been initialized!"

newModels' :: Model -> Tables -> [Frame] -> Int -> Maybe [(Model, Tables, Int)]
newModels' _ _ [] _ = Just []
newModels' model queue (frame:frames) counter = 
    do
      currentFrame <- newModelsForFrame model queue frame counter
      otherFrames  <- newModels model queue frames counter
      return (combine currentFrame otherFrames)


{- Combines the outputs of newModels -}
combine :: [(Model, Tables, Int)] -> [(Model, Tables, Int)]
           -> [(Model, Tables, Int)]
combine new old = 
    if null old
    then new
    else [(mergeModels nm om, mergeSets nq oq, max nc oc)
          | (nm, nq, nc) <- new
         , (om, oq, oc) <- old]
    where mergeModels (Model tbls1 provs1) (Model tbls2 provs2) = 
              Model (mergeSets tbls1 tbls2) (Map.unionWith (++) provs1 provs2)



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
deduceForFrame :: Int -> Model -> Frame -> ([[Obs]], Int)
deduceForFrame counter model frame@(Frame _ body head vars _)
    -- Salman: this may break!
   | null body && not (any (holds model vars) head) = 
       deduceForFrameHelper counter model vars head
   | otherwise = ([], counter)

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
deduce counter model vars hs =
    deduceHelper counter model vars hs
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
            -- are existentials: instantiate the variables with fresh constants; 
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
    let Frame id b h v o = liftTerm (lift sub) frame
    in  Frame id b h (v \\ Map.keys sub) o

{- Helper Functions -}
{- make a new constant (special element starting with "a") -}
makeFreshConstant :: Int -> Term
makeFreshConstant counter = Fn ("a" ++ (show counter)) []
--Elm ("a" ++ show counter)

{- Processes the problems in the pool. Applies a chase step to the problem 
   chosen by selectProblem.It schedules the problems created by this chase
   step. -}
process :: ProbPool [Problem]
process = do
      prob <- selectProblem -- select a problem from the pool
      State.lift $ logM "prob" prob
      case prob of
        Nothing -> return [] -- no more problems
        Just p  -> 
            do
              let newProbs = newProblems p
              --State.lift $ logM "Problems" newProbs
              case newProbs of
                Nothing -> process
                Just [] -> do
                  rest <- process
                  return (p:rest)
                Just ps -> do 
                  mapM scheduleProblem ps
                  process

{- Applies a chase step to the input problem. -}
newProblems :: Problem -> Maybe [Problem]
newProblems (Problem _ _ [] _ _) = Just []
newProblems problem@(Problem frames model (queue:queues) lastID lastConst) =
    case newModels model queue frames lastConst of
      Nothing     -> Nothing
      Just []     -> Just []
      Just models -> Just $ map (\(m, q, c) -> 
                                 Problem { problemFrames       = frames
                                         , problemModel        = m
                                         , problemQueue        = queues ++ [q]
                                         , problemLastID       = lastID
                                         , problemLastConstant = c}) models


-- Three helpers for newProblems:
-- Salman: add comments!
newModels :: Model -> Tables -> [Frame] -> Int -> Maybe [(Model, Tables, Int)]
newModels _ _ [] _ = Just []
newModels model queue (frame:frames) counter = 
    do
      currentFrame <- newModelsForFrame model queue frame counter
      otherFrames  <- newModels model queue frames counter
      return (currentFrame <|> otherFrames)

newModelsForFrame :: Model -> Tables -> Frame -> Int -> 
                     Maybe [(Model, Tables, Int)]
newModelsForFrame model queue frame counter = 
    case newFacts counter model queue frame of
      Nothing         -> Nothing
      Just ([], _, _) -> Just []
      Just (os, c, p) -> Just $ (\o -> Model.add model c o p) <$> os
                      

newFacts :: Int -> Model -> Tables -> Frame -> Maybe ([[Obs]], Int, Maybe Prov)
newFacts counter model queue frame = 
    if   null subs
         -- It sucks that we have to check this here!
    then Just $ ([], counter, Nothing)
    else if (null.frameHead) frame 
         then Nothing
         else Just $ glue (deduceForFrameHelper counter model vars heads)
                          (Just prov)
    where subs    = matchFrame (modelTables model) queue (frameRelInfo frame)
          theSub  = head subs
          lifted  = liftFrame model theSub frame
          heads   = frameHead lifted
          vars    = frameVars lifted
          prov    = (frameID frame, theSub)
          glue    = \(x, y) z -> (x, y, z)
          -- For now just use the first sub and drop the rest!
          -- Also, let's just lift the body for now!
          -- Salman: Separate provenance computation from the main computation.

doChase thy  = chase  $ map parseSequent thy
doChase' thy = chase' $ map parseSequent thy