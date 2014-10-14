{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

{- Razor
   Module      : Chase.HerbrandBase.RelAlg.IHerbrandBase
   Description : Implements a HerbrandBase structure based on relational algebra
   Maintainer  : Salman Saghafi -}

module Chase.HerbrandBase.RelAlg.IHerbrandBase where

--Standard
import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import qualified Data.Vector as Vect
import Data.Vector ((!))
import Data.Maybe
import Data.List (nub)
import Data.Either

-- Control
import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as State

-- Syntax
import Syntax.GeometricUtils

-- Common
import Common.Data ( SequentLike (..) )
import Common.Observation (ObservationSequent, buildObservationSequent)
import Common.Provenance

-- Chase
import Chase.Data

-- RelAlg
import Chase.HerbrandBase.RelAlg.DB as DB (Set (..))
import Chase.HerbrandBase.RelAlg.Lang
import Chase.HerbrandBase.RelAlg.Translate 
    ( bodyRelExp, headRelExp, delta, evaluateRelExp, evaluateRelExpNoDelta
    , tupleTransformer, insertTuples )

-- SAT
import SAT.Data (SATAtom (..))

-- Tools
import Tools.Config (Config (..))
import qualified Tools.ExtendedSet as ExSet

import Tools.Trace

unitName                 = "Chase.HerbrandSet.RelAlg.HerbrandBase"
error_TblEmptyInBody     = "TblEmpty cannot appear in the body of a sequent"
error_TblFullInHead      = "cannot compare to TblFull"
error_TblEmptyInHead     = "cannot compare to TblEmpty"
error_freeVarInHead      = "the expression in head has extra attributes"
error_noValueForConstant = "the constant has no value in the database"

{- TransFunc is the type of a map function that transforms a tuple in the body
   regular expression to a tuple in a head regular expression.
-}
type TransFunc = (Tup -> Tup)

{-| RelSequent stores information about a sequent in relational algebra. 

 [@geoSequent@] is the original geometric 'Sequent' for this relational sequent.
 [@relSequentBody@] is an instance of 'RelExp' that captures the body of a 
 sequent.
 [@relSequentHead@] is a list of pairs of relational expressions and a map 
 function that converts a tuple in @relSequentBody@ to a tuple in the head 
 expression. Each disjunct in the head corresponds to an element in this list.
 [@relSequentBodyDelta@] is a 'RelExp', corresponding to an expression that 
 computes the changes in the body of the sequent incrementally 
 [@relSequentBodyRefs@] is a list of 'TableRef's, containing references to the
 tables that are addressed by the body of 'RelSequent'
 [@relSequentOrigin@] holds the original 'Sequent' instance that hase been
 converted to this 'RelSequent'.
 [@relSequentExists@] maintains a list of Skolem functions that have been 
 assigned to the existential quantifiers of 'relSequentOrigin'. 
 [@relSequentConstants@] maintains a list of 'Constant's in 'relSequentOrigin'.
 -}
data RelSequent = 
    RelSequent { relSequentBody       :: RelExp
               , relSequentHead       :: [(RelExp, TransFunc)]
               , relSequentBodyDelta  :: RelExp
               , relSequentBodyRefs   :: [TableRef]
               , relSequentOrigin     :: Sequent
               , relSequentExists     :: [Either FnSym (FnSym, Atom)]
               , relSequentConstants  :: [Constant]
               }

instance Show RelSequent where
    show (RelSequent bdy hd _ _ _ _ _) = 
        (show bdy) ++ " => " ++ (show (fst <$> hd))

{-| The result of evaluating a 'RelSequent' in a 'RelHerbrandBase' is of type 
  'RelResultSet'. 
  [@allResultTuples@] contains all tuples that make the body of the sequent true
  in the base. This table is used for constructing 'ObservationSequent's. 
  [@newResultTuples@] for each disjunct in the head of the sequent, 
  contains all tuples for which the body of the 'RelSequent' is true but its 
  head is not. Every 'TuplePair' in this set pairs a tuple that makes the body 
  of the 'RelSequent' true and a 'Tuple' that is a transformation of the first 
  tuple, compatible with the head of the sequent. The first tuple of each pair 
  is used for provenance construction and the second tuple is used for pushing
  information into the head. -}
data RelResultSet = RelResultSet 
    { allResultTuples :: TableSub
    , newResultTuples :: [TablePair]
    } deriving (Show, Eq)

{- RelSequent acts like a sequent -}
instance HerbrandBase Database where
    emptyBase              = emptyDatabase
    emptyBaseWithConstants = emptyDatabaseWithConstants
    nullBase               = nullDatabase
    unionBases             = unionDatabases
    diffBases              = diffDatabases
    baseSize               = databaseSize

{- RelSequent acts like a sequent -}
instance SequentLike RelSequent where
    toSequent        = relSequentOrigin
    fromSequent      = buildRelSequent
    failSequent      = failRelSequent
    startSequent     = startRelSequent
    skolemFunctions  = relSequentExists
    sequentConstants = relSequentConstants

instance HerbrandImpl Database RelSequent RelResultSet where
    relevant               = relevantRelSequent
    pull                   = evaluateRelSequent
    push                   = insertRelSequent
    observationalInstances = relSequentInstances


-- the database as the relational Herbranad base:
type RelHerbrandBase = Database

{- Builds an instance of 'RelSequent' from a 'Sequent'. -}
buildRelSequent :: Sequent -> RelSequent
buildRelSequent seq@(Sequent bdy hds)  = 
    let bdyExp = bodyRelExp bdy
        hdsExp = headRelExp hds
        funcs  = trans bdyExp <$> hdsExp
    in  RelSequent { relSequentBody      = bdyExp
                   , relSequentHead      = zip hdsExp funcs
                   , relSequentBodyDelta = delta bdyExp
                   , relSequentBodyRefs  = relExpRefs bdyExp
                   , relSequentOrigin    = seq
                   , relSequentExists    = formulaExistentials hds 
                                           -- sequentExistentials seq
                   , relSequentConstants = constants seq
                   }
    where trans TblFull _  = id
          trans _ TblEmpty = id
          trans b h        = tupleTransformer (header b) (header h)

startRelSequent :: RelSequent -> Bool
startRelSequent seq = relSequentBody seq == TblFull

failRelSequent :: RelSequent -> Bool
failRelSequent seq =  
    (length hds) == 1 && (fst.head) hds == TblEmpty
    where hds = relSequentHead seq

{- Returns true if a the input relational sequent may fire because of the last
  changes in the database represented given as the input database instance.  -}
relevantRelSequent :: RelSequent -> Database -> Bool
relevantRelSequent seq dlt = refsInDatabase (relSequentBodyRefs seq) dlt

{- Evaluates a 'RelSequent' in a database @db@ and a database @dlt@ 
  containing the last set of changes in the database and returns a set of tuples
  that are true in the body of the sequent but not true in its head.
-}
evaluateRelSequent :: RelSequent -> Database -> Database 
                   -> PullM Database RelResultSet
evaluateRelSequent seq@(RelSequent bdy hds bdyDlt _ _ _ _) db dlt = do  
  provs <- liftPullMProvs State.get
  uni <- liftPullMBase State.get
  let bdyDltExTbl      = evaluateRelExp db dlt bdyDlt
  let bdyDltTbl@(DB.Set bdyDltSet) 
                       = undecorateTable bdyDltExTbl

  let hdTbls           = map (\(hd, tran) ->
                              if   bdyDlt == TblFull || 
                                       (header bdyDlt) == fullTableHeader
                              then decorateTable bdyDltTbl Vect.empty
                              else DB.Set $ ExSet.map 
                                       (\(Tuple t _) -> Tuple t (tran t))  
                                       bdyDltSet) hds
                         -- hdTbls contains the head expression, transformation
                         -- of body table in a way that it matches with the
                         -- schema of the head and the entire head table in 
                         -- uni database.

  return $ RelResultSet bdyDltExTbl hdTbls


{- Given a 'RelSequent' and a list of 'Table's resulting from evaluating the
   input 'RelSequent' in the database, inserts the data in the resulting tables
   to the database.
-}
insertRelSequent :: (SATAtom t) => RelSequent -> RelResultSet -> Database 
                 -> PushM Database t Database
insertRelSequent seq resSet db = do
  let hds   = relSequentHead seq
  let tbls  = newResultTuples resSet
  cfg       <- liftPushMConfig State.get
  let body  = relSequentBody seq
  let vars  = case body of
                TblEmpty -> []
                TblFull  -> []
                _        -> Map.keys $ header body
  liftPushMProvs $ State.modify 
                 $ \(id, _, ps) -> (id, vars, ps) 
  let depth = configSkolemDepth cfg
  result    <- (liftM removeEmptyTables)              
               $ foldM ( \d (e, t) -> insertTuples t e d depth) db 
               $ zip (fst <$> hds) tbls
               -- Fold the deduce facts for all heads- 
               -- eventually remove references to empty tables
  (_, _, provs) <- liftPushMProvs State.get
  uni       <- liftPushMBase  State.get
  propThy   <- liftPushMSATTheory State.get


  -- MONITOR
  -- Is @db@ enough or we should use @uni@ or even @unionDatabases uni result@?
  let propSeqs = relSequentInstances seq uni result resSet provs
  -- MONITOR

  let propThy' = foldr (flip storeSequent) propThy propSeqs

  liftPushMSATTheory (State.put propThy')
  return result

filterTable :: Table -> Table
filterTable t = if t == tableFromList [[]]
                then tableFromList []
                else t

{- Creates 'ObservationSequent's for an input 'RelSequent' incrementally. 
   Inputs:
   - 

   Outputs:
   -
-}
relSequentInstances :: RelSequent -> Database -> Database -> RelResultSet 
                    -> ProvInfo -> [ObservationSequent]
relSequentInstances relSeq uni new resSet provs = 
    let subs = createSubs relSeq uni new (allResultTuples resSet) provs
    in  nub [ fromJust inst | 
              (s, bs, es) <- subs
            , let inst =  buildObservationSequent 
                         (instantiateSequent uni new s bs es seq)
            , isJust inst ]
    where seq = toSequent relSeq

instantiateSequent :: Database -> Database -> Sub -> ExistsSub
                   -> Maybe ExistsSub -> Sequent -> Sequent
instantiateSequent uni new sub bodySub exSub seq = 
    let bdy  = formulaExistsSubstitute bodySub (sequentBody seq)
        seq' = substitute sub seq { sequentBody = bdy }
    in  case exSub of
          -- < MONITOR
          Nothing -> Sequent (sequentBody seq') 
                     (Atm $ Rel "Incomplete" [Elem $ Element "e^incomplete"])
          -- MONITOR >
          Just s  -> let seq''      = sequentExistsSubstitute s seq'
                         loneSkFuns = rights $ skolemFunctions seq''
                     in  applyLoneSubs uni new (Map.fromList loneSkFuns) seq''

createSubs :: RelSequent -> Database -> Database -> TableSub
           -> ProvInfo -> [(Sub, ExistsSub, Maybe ExistsSub)]
createSubs seq uni new (DB.Set set) provs = 
    if   bodyExp == TblFull
    then (\(Tuple tup exSub) -> 
              ( emptySub
              , exSub
              , case createExistsSub tup elmProvs skFuns of
                  -- < MONITOR
                  Nothing -> Nothing -- Just Map.empty
                  -- Nothing -> Just exSub
                  -- MONITOR >
                  Just es -> Just es)) <$> ExSet.toList set
    else (\(Tuple tup exSub) -> 
              ( createSub tup heads
              , exSub
              , case createExistsSub tup elmProvs skFuns of
                  -- < MONITOR
                  Nothing ->  Nothing -- Just Map.empty
                  -- Nothing -> Just exSub
                  -- MONITOR >
                  Just es -> Just es)) <$> ExSet.toList set
    where elmProvs = elementProvs provs
          bodyExp  = relSequentBodyDelta seq
          heads    = header bodyExp
          skFuns   = lefts $ skolemFunctions seq


createSub :: Tup -> Header -> Sub
createSub tup heads = Map.map (\i -> Elem (tup ! i)) heads

createExistsSub :: Tup -> ElementProvs -> [FnSym] -> Maybe ExistsSub
createExistsSub tup elmProvs skFuns =     
    let paramProvs = head <$>  -- any of the existing provenance terms works
                     (\e -> getElementProv e elmProvs) <$> 
                     Vect.toList tup
        provsToBe  = (\sk -> if   null paramProvs
                             then Cons (Constant sk)
                             else Fn sk paramProvs) <$> skFuns
        skElems    = zip skFuns $ 
                     (\t -> findElementWithProv t elmProvs)
                     <$> provsToBe
    in  if   all (isJust.snd) skElems
        then Just $ Map.fromList $ (Elem . fromJust <$>) <$> skElems
        else Nothing


applyLoneSubs :: Database -> Database -> Map.Map FnSym Atom -> Sequent
                 -> Sequent
applyLoneSubs uni new skMap seq = 
    if   Map.null skMap
    then seq
    else let subPairs = 
                 Map.elems $ Map.mapWithKey (\k a@(FnRel _ ts) 
                                     -> let e       = lookupElement a
                                            (Var v) = last ts 
                                        in  ( Map.singleton v (Elem e)
                                            , Map.singleton k (Elem e))) 
                 completeAtoms
             (subList, exSubList) = unzip subPairs
             sub                  = Map.unions subList
             rest'                = Map.map (substitute sub) rest
             existsSub'           = Map.unions exSubList
         in  applyLoneSubs uni new rest' 
                 $ sequentExistsSubstitute existsSub' seq
    where (completeAtoms, rest) = Map.partition completeAtom skMap
          completeAtom (FnRel _ ts) = not $ any isVariable (init ts)
          lookupElement atm = 
              case (lookupElementInDB atm uni) of
                Just e  -> e
                Nothing -> case (lookupElementInDB atm new) of
                             Just e  -> e
                             Nothing -> error $ "function value not found"
          lookupElementInDB (FnRel f ts) db = 
              let ref = case ts of
                          [_] -> ConstTable (Constant f)
                          _   -> FnTable f
              in case Map.lookup ref db of
                   Nothing           -> Nothing
                   Just (DB.Set set) -> 
                       let tups = 
                               ExSet.filter (\(Tuple es _) -> 
                                             ((\e -> Elem e) <$> 
                                              (init (Vect.toList es))) 
                                             == (init ts)) set 
                       in  if   ExSet.null tups
                           then Nothing
                           else Just $ (\(Tuple es _) -> Vect.last es) 
                                     $ ExSet.findMin tups
