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
import qualified Chase.Problem.RelAlg.Operations as OP -- remove this
import Chase.Problem.RelAlg.RelAlg as RA

-- Other Modules
import Utils.Utils (allMaps, prodList, allSublists, allCombinations)
import Debug.Trace
import Tools.Logger

{-| This is the main chase function, which tries to find the set of all the 
  models for a given geometric theory.
-}
chase :: Theory -> [Model]
chase thy = 
    -- (trace.show) log
    map problemModel $ probs
    where problem           = buildProblem (relConvert thy) 
          -- Create the initial problem (get rid of function symbols)
          writer            = State.runStateT run [] 
                              -- the wrapper Writer monad for logging
          ((probs, _), log) = Writer.runWriter writer
                              -- use the log information when needed
          run               = scheduleProblem problem >>= (\_ -> 
                              process) -- schedule the problem, then process it

{-| Like chase, but returns only the first model found.
-}
chase' :: Theory -> Maybe Model
chase' thy = Maybe.listToMaybe $ chase thy

{- Given an input frame and a list of models together with new records being 
   added to each model (as the result of applying this function on previous 
   frames, returns a list of models together with new records that are just 
   added to them.
   Algorithmically, the function deduces *a list of list of new facts* for the
   frame, then if the list is not empty, it creates a list of new models by
   adding each list of facts to the original model.
-}
mapModels :: Frame -> [(Model, Tables, Int)] -> [(Model, Tables, Int)]
mapModels f [] = []
mapModels f ms = 
    foldr combine [] ms
    where combine (m, rs, c) res = 
              let (os, c') = deduceForFrame c m f
              in if null os                                            
                 then (m, rs, c):res
                 else let ms' = map (Model.add m c') os
                      in  (map (\(m', rs', c'') -> 
                                    (m', mergeSets rs rs', c'')) ms') ++ res

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
      case prob of
        Nothing -> return [] -- no problems
        Just p  -> 
            do
              let newProbs = newProblems p -- map the problem (in MapReduce)
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
newProblems problem@(Problem frames model lastID lastConst) =
    case newModels frames model lastConst of
      Nothing     -> Nothing
      Just []     -> Just []
      Just models -> Just $ map (\(m, _, c) -> 
                                 Problem { problemFrames       = frames
                                         , problemModel        = m
                                         , problemLastID       = lastID
                                         , problemLastConstant = c}) models

-- Three helpers for newProblems:
-- Salman: add more explanation.
newModels :: [Frame] -> Model -> Int -> Maybe [(Model, Tables, Int)]
newModels [] _ _ = Just []
newModels (frame:frames) model counter = 
    do
      currentFrame <- newModelsForFrame frame model counter
      otherFrames  <- newModels frames model counter
      return (currentFrame <|> otherFrames)

newModelsForFrame :: Frame -> Model -> Int -> 
                     Maybe [(Model, Tables, Int)]
newModelsForFrame frame model counter = 
    case newFacts counter model frame of
      Nothing      -> Nothing
      Just ([], _) -> Just []
      Just (os, c) -> let ms = map (Model.add model c) os
                      in Just $ map (\(m, rs, c') -> (m, rs, c')) ms

newFacts :: Int -> Model -> Frame -> Maybe ([[Obs]], Int)
newFacts counter model frame = 
    -- trace "--------------------"
    -- (trace.show) model
    -- (trace.show) frame
    -- (trace.show) subs
    -- trace "===================="
    -- $
    if   null subs
         -- It sucks that we have to check this here!
    then Just $ ([], counter)
    else if (null.frameHead) frame 
         then Nothing
         else Just $ deduceForFrameHelper counter model vars heads
    where subs    = matchFrame (frameRelInfo frame) (modelTables model)
          lifted  = liftFrame model (head subs) frame
          heads   = frameHead lifted
          vars    = frameVars lifted
          -- For now just use the first sub and drop the rest!
          -- Also, let's just lift the body for now!

doChase thy  = chase  $ map parseSequent thy
doChase' thy = chase' $ map parseSequent thy


-- creates too many isomorphic modesl:
-- testThy = ["P(a()) & P(b()) & f(a()) = b() & f(b()) = a()",
--           "P(x) => Q(b()) | Q(f(x))"] 