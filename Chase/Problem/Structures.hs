{-|
  This module defines the underlying data-structures that form for a chase 
  problem. Conceptually, a problem is a branch of chase execution with its 
  own result (whether it has a model or it is unsatisfiable).
-}

module Chase.Problem.Structures where

-- General Modules
import Data.List
import qualified Data.Map as Map
import Control.Monad
import qualified Control.Monad.RWS.Lazy as RWS
import qualified Control.Monad.State.Lazy as State
import Control.Applicative

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities(TermBased(..))
import Tools.Config as Config
import Tools.Counter as Counter

-- Chase Modeuls:
import Chase.Problem.BaseTypes
import Chase.Problem.Model
import Chase.Problem.RelAlg.RelAlg
import qualified Chase.Problem.RelAlg.Operations as OP
import qualified RelAlg.DB as RA

{-| Additional information about frames are stored as frame type instances. -}
data FrameType = FrameType { ftInitial      :: Bool
                           -- has empty left
                           , ftFail         :: Bool
                           -- has empty right
                           , ftExistential  :: Bool 
                           -- contains existential on right
                           , ftDisjunction  :: Bool
                           -- contains disjunction on right
                           , ftUniversal    :: Bool
                           -- contains free variables on right
                           } deriving Show

untypedFrame :: FrameType
untypedFrame =  FrameType False False False False False

unionFrameTypes :: FrameType -> FrameType -> FrameType
unionFrameTypes ft1@(FrameType i1 f1 e1 d1 u1) ft2@(FrameType i2 f2 e2 d2 u2) =
    FrameType (i1 || i2) (f1 || f2) (e1 || e2) (d1 || d2) (u1 || u2)

hasFrameType :: Frame -> [FrameType -> Bool] -> Bool
hasFrameType frame types = and $ (flip ($)) (frameType frame) <$> types


{- Since field selectors of FrameType are frequently used to identify frames 
   for scheduling, it is convenient to define them as a type. -}
type FrameTypeSelector = FrameType -> Bool

{- FrameScheduleInfo keeps the information required to schedule frames for a 
   problem:
   - problemSelectors:  set of selctors for the current problem
   not existential sequents.
   - problemBigStepAge: age of the problem (number of firing existentials)
   - problemCollapses: keeps track of the number of collapses permitted on this
   problem (in fact, its the problem's model), which grows logarithmically with
   the size of the model.
   - problemExtendable: determines whether new elements can be added to the
   domain of the problem or not.
 -}
data ScheduleInfo = ScheduleInfo { problemSelectors  :: [[FrameTypeSelector]]
                                 , problemBigStepAge :: Int
                                 , problemCollapses  :: Int
                                 , problemExtendable :: Bool }
instance Show ScheduleInfo where
    show (ScheduleInfo _ age _ _) =
        "-- AGE:\n" ++ (show age) ++ "\n"



{- A shorthand for a systematic way of scheduling frames in the following order:
   1- Process frames with empty right
   2- Process frames with no disjunctions and no existential qunatifiers 
   3- Process frames with disjunctions 
   4- Process framed with existential quantifiers -}
allFrameTypeSelectors :: [[FrameTypeSelector]]
allFrameTypeSelectors = 
    [failFrames, regularFrames, disjunctFrames, existFrames]
    where failFrames      = [ftFail]
          regularFrames   = [not.ftFail, not.ftDisjunction, not.ftExistential]
          disjunctFrames  = [ftDisjunction]
          existFrames     = [ftExistential]

{-| Relational information for a sequent, correspoinding to a frame:
  - bodyExp: a relational expression corresponding to the body of a sequent.
  - bodyLbls: the labels (column headers) for the expression, corresponding to
  the body.
  - headExp: a relational expression corresponding to the head of a sequent.
  - headLbls: the labels (column headers) for the expression, corresponding to
  the head.
 -}
data RelInfo = RelInfo {
      bodyExp      :: (RelExp, Labels),
      bodyDeltaExp :: (RelExp, Labels),
      headExp      :: [(RelExp, Labels)]
} deriving Show

{-| Frame is a data structure corresponding to a geometric sequent in a problem.
  A Frame structure consists of the following parts:
  - frameID: a unique ID for every Frame
  - frameBody: a list of obs corresponding to the formula on left of a sequent.
  Every obs is equivalent to an atomic formula as a conjunct on the left of the 
  sequent.
  - frameHead: a list of list of obs corresponding to the formula on right of a 
  sequent. The obs in the top level list are assumed to be disjuncted while the 
  obs within every inner list are conjuncted.
  - frameVars: a list of free variables (universally quantified) that appear in 
  the sequent.
  - frameRelInfo: relational information for the frame.  
  - frameType: contains additional information about the right of the sequent.
-}
data Frame = Frame { 
      frameID        :: ID
    , frameBody      :: [Obs]
    , frameHead      :: [[Obs]]
    , frameVars      :: Vars
    , frameRelInfo   :: RelInfo
    , frameType      :: FrameType }

instance Show Frame where
    show (Frame id body head vars _ _) = 
        (show id) ++ ": " ++ (show body) ++ " => " ++ (show head) ++ "(" 
                      ++ (show vars) ++ ")"


instance Eq Frame where
    (Frame _ b1 h1 _ _ _) == (Frame _ b2 h2 _ _ _) =
        b1 == b2 && h1 == h2


instance TermBased Frame where
    liftTerm f (Frame id body head vars relInfo fType) = 
        Frame { frameID        = id
              , frameBody      = map (liftTerm f) body
              , frameHead      = map (liftTerm f) head
              , frameVars      = vars
              , frameRelInfo   = relInfo
              , frameType      = fType }
    freeVars = frameVars

{-| A problem represents keeps track of a branch of computation, which contains 
  a geometric theory (a set of sequents) and a model. It also has a queue of 
  deduced facts that are waiting to be processed.
  - problemFrames: a list of frame IDs corresponding to the sequents of the 
  problem.
  - problemModel: a model corresponding to the problem.
  - problemQueue: a queue of deduced facts in form of a set of delta tables to
  be processed.
  - problemSymbols: a list of symbols that appear in the left of frames.
  - problemScheduleInfo: keeps track of information that is required to 
  schedule frames for the problem. This field keeps track of a list of frame 
  type selectors to determine the type of frames that can be scheduled next.
  - probelmLastConstant: keeps track of an index used to create a new element 
  for satisfying an existential quantifier.
-}
data Problem = Problem {
      problemID           :: ID, 
      problemFrames       :: [ID],
      problemModel        :: Model,
      problemQueue        :: [Tables],
      problemScheduleInfo :: ScheduleInfo,
      problemLastConstant :: Int -- We may remove this later
}

instance Eq Problem where
    p == p' = problemID p == problemID p'

instance Show Problem where
    show (Problem id frames model _ sched _) =
        "-- ID:\n" ++ (show id) ++ "\n" ++
        "-- SCHED:\n" ++ (show sched) ++ "\n" ++
        "-- FRAMES:\n" ++ (show frames) ++ "\n" ++
        "-- MODEL: \n" ++ (show model) ++ "\n"

{-| A map from frame IDs to frames. -}
type FrameMap = Map.Map Int Frame

{-| ProbPool keeps track of a counter for problem IDs,  theory and the pool 
  of problems.
-}
type CntrCfg   = CounterT ConfigMonad
type ProbPool = RWS.RWST [String] [String] 
     (FrameMap, [Problem])  -- Frames and Problems
     CntrCfg                -- Counter and Config

liftCounter :: (Monad m, State.MonadTrans t) => m a  -> t m a
liftCounter  = RWS.lift
liftConfig :: ( Monad (t m), Monad m, State.MonadTrans t'
              , State.MonadTrans t) => m a  -> t' (t m) a
liftConfig  = liftCounter.State.lift