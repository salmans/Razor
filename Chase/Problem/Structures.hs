{-|

   This module defines the underlying data-structures that form for a chase problem. Conceptually, a problem is a branch of chase execution with its own result (whether it has a model or it is unsatisfiable).

-}
{-# LANGUAGE GADTs #-}

module Chase.Problem.Structures where

-- General Modules
import Data.List
import qualified Data.Map as Map
import qualified Control.Monad.State as State
import qualified Control.Monad.Writer as Writer
import Control.Applicative

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities(TermBased(..))

-- Chase Modeuls:
import Chase.Problem.Observation
import Chase.Problem.Model
import qualified CC.CC as CC
import qualified RelAlg.DB as RA

import Debug.Trace

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
      frameVars :: Vars,
      frameOrig :: Sequent
}


instance Show Frame where
    show (Frame id body head vars _) = 
        (show id) ++ ": " ++ (show body) ++ " => " ++ (show head) ++ "(" 
                      ++ (show vars) ++ ")"


instance Eq Frame where
    (Frame _ b1 h1 _ _) == (Frame _ b2 h2 _ _) =
        b1 == b2 && h1 == h2


instance TermBased Frame where
    liftTerm f (Frame id body head vars seq) = 
        Frame { frameID = id
              , frameBody = map (liftTerm f) body
              , frameHead = map (liftTerm f) head
              , frameVars = vars
              , frameOrig = seq
              }

    freeVars = frameVars

{-| SymbolMap is a map from every symbol (relation symbol or function symbol) to the IDs of frames in whose body they appear and the ordinal of the term in the body.
-}
type SymbolMap = Map.Map Sym [(ID, Int)]

{-| The new rewrite rules that are added to the model will be queued in a queue until their impact is pushed back to the list of frames.
-}
type Queue = [CC.RWRule]

{-| A problem represents keeps track of a branch of computation, which contains a geometric theory (a set of sequents) and a model. It also has a queue of deduced facts that are waiting to be processed.
  - problemFrames: a list of frames corresponding to the sequents of the problem.
  - problemModel: a model corresponding to the problem.
  - problemQueue: a queue of the deduced facts to be processed.
  - problemSymbols: a list of symbols that appear in the left of frames.
  - problemLastID: a convenient way of keeping track of the last ID assigned to the frames. This is used when new frames get instantiated.
  - probelmLastConstant: keeps track of an index used to create a new element for satisfying an existential quantifier.
-}
data Problem = Problem {
      problemFrames       :: [Frame],
      problemModel        :: Model,
      problemQueue        :: Queue,
      problemSymbols      :: SymbolMap,
      problemLastID       :: ID,
      problemLastConstant :: Int -- We may remove this later
}

instance Show Problem where
    show (Problem frames model queue symbols _ _) =
        "-- FRAMES:\n" ++ (show frames) ++ "\n" ++
        "-- MODEL: \n" ++ (show model) ++ "\n" ++
        "-- QUEUE: \n" ++ (show queue) ++ "\n" ++
        "-- SYMBOLS: \n" ++ (show symbols) ++ "\n"

{-| ProbPool keeps track of the pool of problems. -}
-- Currently, ProbPool is a Writer monad, as a logger, on top of
-- a State monad, for the list of problems.
type ProbPool = State.StateT [Problem] (Writer.Writer [String])

type Logger   = Writer.Writer [String]