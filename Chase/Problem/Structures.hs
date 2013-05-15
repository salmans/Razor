{-| Time-stamp: <2013-05-14 19:45:59 Salman Saghafi>

   This module defines the underlying data-structures that form for a chase problem. Conceptually, a problem is a branch of chase execution with its own result (whether it has a model or it is unsatisfiable).

-}
module Chase.Problem.Structures where

-- General Modules
import Data.List
import qualified Data.Map as Map

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities(TermBased(..))

-- Chase Modeuls:
import Chase.Problem.Observation
import Chase.Problem.Model
import qualified CC.CC as CC

{- A unique ID for every frame in the problem -}
type ID = Int

{-| Frame is a data structure corresponding to a geometric sequent in a problem. A Frame structure consists of the following parts:
  - frameID: a unique ID for every Frame
  - frameBody: a list of obs corresponding to the formula on left of a sequent. Every obs is equivalent to an atomic formula as a conjunct on the left of the sequent.
  - frameHead: a list of list of obs corresponding to the formula on right of a sequent. The obs in the top level list are assumed to be disjuncted while the sobs within every inner list are conjuncted.
  - frameVars: a list of free variables (universally quantified) that appear in the sequent.
-}
data Frame = Frame {
      frameID :: ID,
      frameBody :: [Obs],
      frameHead :: [[Obs]],
      frameVars :: Vars
}

instance Show Frame where
    show (Frame id body head vars) = 
        (show id) ++ ": " ++ (show body) ++ " => " ++ (show head) ++ "(" 
                      ++ (show vars) ++ ")"


instance Eq Frame where
    (Frame _ b1 h1 _) == (Frame _ b2 h2 _) =
        b1 == b2 && h1 == h2


instance TermBased Frame where
    liftTerm f (Frame id body head vars) = 
        Frame id (map (liftTerm f) body) (map (liftTerm f) head) vars
    freeVars = frameVars

{-| SymbolMap is a map from every symbol (relation symbol or function symbol) to the IDs of frames in whose body they appear and the ordinal of the term in the body.
-}
type SymbolMap = Map.Map Sym [(ID, Int)]

{-| The deduced new facts that are added to the model will be queued in a queue until their impact is pushed back to the list of frames. In fact, the list is a list of terms equivalent to the deduced facts. 
-}
type Queue = [Obs]

{-| A problem represents keeps track of a branch of computation, which contains a geometric theory (a set of sequents) and a model. It also has a queue of deduced facts that are waiting to be processed.
  - problemFrames: a list of frames corresponding to the sequents of the problem.
  - problemModel: a model corresponding to the problem.
  - problemQueue: a queue of the deduced facts to be processed.
  - problemSymbols: a list of symbols that appear in the left of frames.
  - problemLastID: a convenient way of keeping track of the last ID assigned to the frames. This is used when new frames get instantiated.
  - probelmLastConstant: keeps track of an index used to create a new element for satisfying an existential quantifier.
-}
data Problem = Problem {
      problemFrames :: [Frame],
      problemModel :: Model,
      problemQueue :: Queue,
      problemSymbols :: SymbolMap,
      problemLastID :: ID,
      problemLastConstant :: Int -- We may remove this later
}

instance Show Problem where
    show (Problem frames model queue symbols _ _) =
        "-- FRAMES:\n" ++ (show frames) ++ "\n" ++
        "-- MODEL: \n" ++ (show model) ++ "\n" ++
        "-- QUEUE: \n" ++ (show queue) ++ "\n" ++
        "-- SYMBOLS: \n" ++ (show symbols) ++ "\n"