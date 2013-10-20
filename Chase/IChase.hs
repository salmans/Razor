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

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities
import Tools.GeoUnification

-- Chase Modules
import Chase.Problem.Observation
import Chase.Problem.Structures
import Chase.Problem.Operations
import Chase.Problem.Model(Model (..))
import qualified Chase.Problem.Model as Model
import qualified CC.CC as CC -- remove this
import Chase.Problem.IRelAlg as RA

-- Other Modules
import Utils.Utils (allMaps, prodList, allSublists, allCombinations)
import Debug.Trace
import Tools.Logger

{-| This is the main chase function, which tries to find the set of all the models for a given geometric theory.
-}
chase :: Theory -> [Model]
chase thy = 
--    (trace.show) log
    map problemModel $ probs
    where problem           = buildProblem (relConvert thy) 
          -- Create the initial problem (get rid of function symbols)
          writer            = State.runStateT run [] 
                              -- the wrapper Writer monad for logging
          ((probs, _), log) = Writer.runWriter writer
                              -- use the log information when needed
          run               = scheduleProblem problem >>= (\_ -> 
                              processRA) -- schedule the problem, then process it

{-| Like chase, but returns only the first model found.
-}
chase' :: Theory -> Maybe Model
chase' thy = Maybe.listToMaybe $ chase thy

{- Applies mapProblem and reduceProblem on a pool (list) of problems and 
   returns a list of problems that cannot be mapped or reduced anymore. 
   The models of the processed problems are the models that the chase 
   is looking for.
-}
-- process :: ProbPool [Problem]
-- process = do
--       prob <- selectProblem -- select a problem from the pool
--       case prob of
--         Nothing   -> return [] -- no problems
--         Just p -> 
--             do
--               let newProbs = mapProblem p -- map the problem (in MapReduce)
--               let (processed, unprocessed) = 
--                       partition (null.problemQueue) newProbs
--                   -- If any of the mapped problems have something in their
--                   -- queue, they must be reduced. Mapped problems with empty
--                   -- queues are done!
--               let reduced = map reduceProblem unprocessed                  
--                   -- Reduce the problems that are not done processing yet.
--               mapM scheduleProblem reduced
--                   -- Schedule the reduced problems in the pool.
--               ps <- process -- Recursive call for furhter processing
--               return $ processed ++ ps

{- Maps a problem to another problem (possibly a list of problems) by
   branching over the right of the frames whose bodies are empty. For each
   disjunct on right of a frame with an empty body, we create a new problem
   that contains new models and add the new rewrite rules for each model 
   to the problem's queue. Consequently, the frame, with an empty body, will 
   be removed from the problem.
   If any of the frames of the input problem is empty on both left and right,
   the chase fails in this branch.

   Note: it has been assumed that this function is called only on problems with
   empty queues. Otherwise, the initial queue has to be added to the queues of 
   each branch
-}
-- mapProblem :: Problem -> [Problem]
-- mapProblem p@(Problem frames model queue syms lastID lastConst) =
--     if any' (\f -> (null . frameBody) f && (null . frameHead) f) frames
--     -- if the theory has a contradictory sequent
--     then [] -- then don't continue this branch
--     else map (\(m, q, c) -> 
--               (Problem nonEmptyFrs m q syms lastID c)) queues
--     where queues = foldr mapModels [(model, [], lastConst)] emptyFrs
--               -- Constructs the queue of rewrite rules and corresponding models
--               -- for each model mapping.
--           (emptyFrs, nonEmptyFrs) = partition (null.frameBody) frames
--               -- get rid of frames that we have already pushed into the model
--           any' pred = foldr (\e r -> pred e || r) False
--               -- Since we extend the frames of a problem by adding new
--               -- frames to the end, a more efficient version of any,
--               -- i.e. eny', starts from the end of the list.

{- Given an input frame and a list of models together with new rewrite rules 
   being added to each model (as the result of applying this function on 
   previous frames, returns a list of models together with their rewrite rules
   that are just added to them.
   Algorithmically, the function deduces *a list of list of new facts* for the
   frame, then if the list is not empty, it creates a list of new models by
   adding each list of facts to the original model.
-}
mapModels :: Frame -> [(Model, [CC.RWRule], Int)] -> [(Model, [CC.RWRule], Int)]
mapModels f [] = []
mapModels f ms = 
    foldr combine [] ms
    where combine (m, rs, c) res = 
              let (os, c') = deduceForFrame c m f
              in if null os                                            
                 then (m, rs, c):res
                 else let ms' = map (Model.add m) os
                      in (map (\(m', rs') -> (m', rs ++ rs', c')) ms') ++ res


{- Reduces a problem by instantiating the frames of the problem with the obs
   in the queue of the problem.
-}
-- reduceProblem :: Problem -> Problem
-- reduceProblem (Problem frames model queue symMap lastID lastConst) =
--     let tempProb = Problem normalFrames model
--                      [] symMap lastID lastConst
--     in extendProblem tempProb newFrames
--     where newFrames = concatMap (\f -> instFrame model symMap f queue)
--                       normalFrames
--                       -- a list of newly instantiated frames          
--           normalFrames = map (normalizeFrame model) frames
--                       -- We normalize old frames as we do for new frames in
--                       -- every instFrame call.


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
deduceHelper counter model@(Model trs domain) vars allObs@(obs:rest)             
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

{- Instantiates a frame according to a list of rewrite rules and returns a list 
   of instantiated frames. This function uses matchFrame to match the frame with
   left of the rules based on narrowing.   
-}
-- instFrame :: Model -> SymbolMap -> Frame -> [CC.RWRule]  -> [Frame]
-- instFrame model symMap frame newRules = 
--     let frames = map (\ss -> applySubList model frame ss) subLists
--     in  map (normalizeFrame model) frames 
--         -- Instantiate the input frame with all lists of substitutions
--         -- and normalize the results.
--     where subLists = filter (not.null) $ allCombinations (nub subs)
--                      -- Create a list of all the possible combinations of 
--                      -- substitutions resulted from matching with each rule
--           subs     = matchFrame model symMap frame newRules
--                      -- Match the input frame with the new rules using narrowing
--                      -- and create a list of substitutions.

{- Normalizes a frame: it replaces every observation in the frame with its 
   normal form in the model. Also, it removes the obs that are observed 
   (rewrite to truth) in the model from the frame's body.
-}
-- normalizeFrame :: Model -> Frame -> Frame   
-- normalizeFrame model frame@(Frame id body head vars orig) = 
--     Frame { frameID   = id 
--           , frameBody = (filter (not.Model.isTrue model) normalizedBody)
--           , frameHead = head
--           , frameVars = vars
--           , frameOrig = orig}
--     where normalizedBody = map (Model.denotes model) body
          
{- Instantiates the input frame by applying all the given substitutions on it.
-}
-- applySubList :: Model -> Frame -> [Sub] -> Frame
-- applySubList model frame subs =
--     foldr (\s f -> liftFrame model s f) frame subs

{- Returns a list of substitutions that match any of the Obs in the body of the
   input frame to the left of the rewrite rules. The underlying matching 
   algorithm is based on narrowing.

   REMARK: a more efficient version of this function may use the input SymbolMap
   to apply narrowing on a list of candidate frames.
-}
-- matchFrame :: Model -> SymbolMap -> Frame -> [CC.RWRule] -> [[Sub]]
-- matchFrame mdl _ frame newRules =
--     map (narrowObs mdl newRules) (frameBody frame)

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

-- Relational Algebra
processRA :: ProbPool [Problem]
processRA = do
      prob <- selectProblem -- select a problem from the pool
      case prob of
        Nothing -> return [] -- no problems
        Just p  -> 
            do
              let newProbs = newProblems p -- map the problem (in MapReduce)
              --State.lift $ logM "Problems" newProbs
              case newProbs of
                Nothing -> processRA
                Just [] -> do
                  rest <- processRA
                  return (p:rest)
                Just ps -> do 
                  mapM scheduleProblem ps
                  processRA


newProblems :: Problem -> Maybe [Problem]
newProblems problem@(Problem frames model queue symMap lastID lastConst) =
    case newModels frames model lastConst of
      Nothing      -> Nothing
      Just []      -> Just []
      Just models  -> Just $ map (\(m, _, c) -> 
                               Problem { problemFrames       = frames
                                       , problemModel        = m
                                       , problemQueue        = queue
                                       , problemSymbols      = symMap
                                       , problemLastID       = lastID
                                       , problemLastConstant = c}) models

newModels :: [Frame] -> Model -> Int -> Maybe [(Model, [CC.RWRule], Int)]
newModels [] _ _ = Just []
newModels (frame:frames) model counter = 
    do
      currentFrame <- newModelsForFrame frame model counter
      otherFrames  <- newModels frames model counter
      return (currentFrame <|> otherFrames)

newModelsForFrame :: Frame -> Model -> Int -> 
                     Maybe [(Model, [CC.RWRule], Int)]
newModelsForFrame frame model counter = 
    case newFacts counter model frame of
      Nothing      -> Nothing
      Just ([], _) -> Just []
      Just (os, c) -> let ms = map (Model.add model) os
                      in Just $ map (\(m, rs) -> (m, rs, c)) ms

newFacts :: Int -> Model -> Frame -> Maybe ([[Obs]], Int)
newFacts counter model frame = 
    if   null subs
         -- It sucks that we have to check this here!
    then Just $ ([], counter)
    else if (null.frameHead) frame 
         then Nothing
         else Just $ deduceForFrameHelper counter model vars heads
    where subs    = RA.matchRA (frameOrig frame) (Model.modelTables model)
          lifted  = liftFrame model (head subs) frame
          heads   = frameHead lifted
          vars    = frameVars lifted
          -- For now just use the first sub and drop the rest!
          -- Also, let's just lift the body for now!


doChase thy = chase $ map parseSequent thy
doChase' thy = chase' $ map parseSequent thy


testThy = ["(src = dest) => Refuse(src,dest)",
           "exists x. exists y. Toll(x,y)"]
