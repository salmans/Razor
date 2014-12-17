{- This file is part of Razor.

  Razor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Razor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Razor.  If not, see <http://www.gnu.org/licenses/>.

  Module      : Chase.PossibleFacts.RelAlg.IPossibleFacts
  Description : Implements a PossibleFacts structure based on relational algebra
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Chase.PossibleFacts.RelAlg.IPossibleFacts where

--Standard
import qualified Data.Map as Map
import qualified Data.Vector as Vect
import Data.Vector ((!))
import Data.Maybe
import Data.List (elemIndex, nub, sortBy)
import Data.Either

-- Control
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.List
import qualified Control.Monad.State as State

-- Syntax
import Syntax.GeometricUtils

-- Common
import Common.Basic (Id)
import Common.Data ( SequentLike (..) )
import Common.Observation ( Observation(..), ObservationSequent
                          , buildObservationSequent)
import Common.Provenance

-- SAT
import SAT.Data ( SATIterator, satStore )

-- Chase
import Chase.Data

-- RelAlg
import qualified Chase.PossibleFacts.RelAlg.DB as DB
import Chase.PossibleFacts.RelAlg.Lang
import Chase.PossibleFacts.RelAlg.Translate 
    ( bodyRelExp, headRelExp, delta, evaluateRelExp, tupleTransformer
    , insertTuples )

-- Tools
import Tools.Config (Config (..))

unitName                 = "Chase.PossibleFacts.RelAlg.PossibleFacts"
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

{-| The result of evaluating a 'RelSequent' in a 'RelPossibleFacts' is of type 
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
instance PossibleFacts Database where
    emptyBase              = emptyDatabase
    emptyBaseWithConstants = emptyDatabaseWithConstants
    nullBase               = nullDatabase
    unionBases             = unionDatabases
    diffBases              = diffDatabases
    baseSize               = databaseSize
    addToBase              = addToDatabase

{- RelSequent acts like a sequent -}
instance SequentLike RelSequent where
    toSequent        = relSequentOrigin
    fromSequent      = buildRelSequent
    failSequent      = failRelSequent
    startSequent     = startRelSequent
    skolemFunctions  = relSequentExists
    sequentConstants = relSequentConstants

instance ChaseImpl Database RelSequent RelResultSet where
    relevant               = relevantRelSequent
    pull                   = evaluateRelSequent
    push                   = insertRelSequent
    observationalInstances = relSequentInstances


-- the database as the relational Herbranad base:
type RelPossibleFacts = Database

{- Builds an instance of 'RelSequent' from a 'Sequent'. -}
buildRelSequent :: Sequent -> Maybe RelSequent
buildRelSequent seq@(Sequent bdy hds)  = 
    let bdyExp = bodyRelExp bdy
        hdsExp = headRelExp hds
        funcs  = trans bdyExp <$> hdsExp
    in  Just RelSequent { relSequentBody      = bdyExp
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
  let bdyDltExTbl = evaluateRelExp db dlt bdyDlt
  let bdyDltTbl   = undecorateTable bdyDltExTbl

  let hdTbls      = map (\(hd, tran) ->
                             if   bdyDlt == TblFull || 
                                      (header bdyDlt) == fullTableHeader
                             then decorateTable bdyDltTbl Vect.empty
                             else DB.map (\(Tuple t _) -> Tuple t (tran t))  
                                  bdyDltTbl) hds
                         -- hdTbls contains the head expression, transformation
                         -- of body table in a way that it matches with the
                         -- schema of the head and the entire head table in 
                         -- uni database.
  return $ RelResultSet bdyDltExTbl hdTbls


{- Given a 'RelSequent' and a list of 'Table's resulting from evaluating the
   input 'RelSequent' in the database, inserts the data in the resulting tables
   to the database.
-}
insertRelSequent :: (SATIterator it) => RelSequent -> RelResultSet -> Database 
                 -> PushM Database it Database
insertRelSequent seq resSet db = do
  let hds   = relSequentHead seq
  let tbls  = newResultTuples resSet
  cfg       <- liftPushMConfig State.get
  let body  = relSequentBody seq
  let vars  = case body of
                TblEmpty -> []
                TblFull  -> []
                _        -> let l = Map.toList $ header body 
                                l' = sortBy (\(_, i1) (_, i2)->compare i1 i2) l
                            in fst <$> l'
  liftPushMProvs $ State.modify 
                 $ \(seqid, _, provs) -> (seqid, vars, provs) --change ps to provs
  let depth = configDefaultSkolemDepth cfg
  let dpths = configSkolemDepth cfg
  result    <- (liftM removeEmptyTables)              
               $ foldM ( \d (e, t) -> insertTuples t e d depth dpths) db 
               $ zip (fst <$> hds) tbls
               -- Fold the deduce facts for all heads- 
               -- eventually remove references to empty tables
  (seqid, _, provs) <- liftPushMProvs State.get 
  uni       <- liftPushMBase  State.get
  iter      <- liftPushMSATIterator State.get


  -- MONITOR
  -- Is @db@ enough or we should use @uni@ or even @unionDatabases uni result@?
  let propSeqs = observationalInstances seq uni result resSet provs
  -- MONITOR

  -- Compute the next iterator and the next blame info for prov in the one fold:
  let (iter', provs') = foldr (\(sub, oseq) (it, pr) ->
                                  let blm = TheoryBlame seqid sub
                                      it' = satStore oseq it
                                      pr' = modifyBlameSequentMap
                                                (addBlameSequent blm oseq) pr
                                  in  (it', pr'))
                        (iter, provs) propSeqs

  liftPushMSATIterator (State.put iter')
  liftPushMProvs $ State.modify
                 $ \(seqid, vars, _) -> (seqid, vars, provs')
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
                    -> ProvInfo -> [(Sub, ObservationSequent)]
relSequentInstances relSeq uni new resSet provs = 
    let subs = createSubs relSeq uni new (allResultTuples resSet) provs
    in nub [ (s, fromJust inst) | 
              (s, bs, es) <- subs
            , inst        <-  buildObservationSequent <$>
                              (instantiateSequent uni new s bs es seq)
            , isJust inst ]
    where seq = toSequent relSeq

instantiateSequent :: Database -> Database -> Sub -> ExistsSub
                   -> Either FnSym ExistsSub -> Sequent -> [Sequent]
instantiateSequent uni new sub bodySub exSub seq = 
    let bdy  = formulaExistsSubstitute bodySub (sequentBody seq)
        seq' = substitute sub seq { sequentBody = bdy }
    in  case exSub of
          Left skFn -> [incompleteSequent (sequentBody seq') skFn]
          Right s   -> let seq''      = sequentExistsSubstitute s seq'
                           loneSkFuns = rights $ skolemFunctions seq''
                           skMap      = Map.fromListWith (++) 
                                      $ (pure <$>) <$> loneSkFuns
                       in  case runListT $ applyLoneSubs uni new skMap seq'' of
                             Left skFn -> [incompleteSequent (sequentBody seq') skFn]
                             Right sqs -> sqs

createSubs :: RelSequent -> Database -> Database -> TableSub
           -> ProvInfo -> [(Sub, ExistsSub, Either FnSym ExistsSub)]
createSubs seq uni new tbl provs =
    let subsOf t = if   bodyExp == TblFull
                   then emptySub
                   else createSub t heads
    in  (\(Tuple tup exSub) -> 
              ( subsOf tup
              , exSub
              , createExistsSub tup elmProvs skFuns)) <$> dblist
    where dblist   = DB.toList tbl
          elmProvs = elementProvs provs
          bodyExp  = relSequentBodyDelta seq
          heads    = header bodyExp
          skFuns   = lefts $ skolemFunctions seq

createSub :: Tup -> Header -> Sub
createSub tup heads = Map.map (\i -> Elem (tup ! i)) heads

createExistsSub :: Tup -> ElementProvs -> [FnSym] -> Either FnSym ExistsSub
createExistsSub tup elmProvs skFuns =     
    let paramProvs = fromJust <$>  -- any of the existing provenance terms works
                     (\e -> getElementProv e elmProvs) <$> 
                     Vect.toList tup
        provsToBe  = (\sk -> if   null paramProvs
                             then Cons (Constant sk)
                             else Fn sk paramProvs) <$> skFuns
        skElems    = zip skFuns $ 
                     (\t -> findElementWithProv t elmProvs)
                     <$> provsToBe
    in  if   all (isJust.snd) skElems
        then Right $ Map.fromList $ (Elem . fromJust <$>) <$> skElems
        else Left  $ fst . head . filter (isNothing.snd) $ skElems
             -- Any of the skFuns that have reached the maximum limit works!

applyLoneSubs :: Database -> Database -> Map.Map FnSym [Atom] -> Sequent
              -> ListT (Either FnSym) Sequent
applyLoneSubs uni new skMap seq =
    if   Map.null skMap
    then return seq -- done!
    else if   Map.null completeAtoms -- no more complete atoms, thus no progress!
         then lift . Left . head . Map.keys $ rest
              -- Any of the unassigned function values can be used for
              -- constructing an "incomplete sequent".
         else do
           let atomSubs skFun a@(FnRel f ts) = do
                 let  Var v = last ts
                 case lookupElement a of
                   Nothing   -> lift $ Left f
                   Just elms -> return $
                                  (\e -> ((v, Elem e), (skFun, Elem e))) <$>
                                  DB.toList elms
           temp            <- mapM (uncurry atomSubs) completeAtomsList
           let res          = transformTuples $ concat temp
           let (sub, exSub) = (\(x, y) -> (Map.fromList x, Map.fromList y)) res
           let rest'        = Map.map (substitute sub) rest
           applyLoneSubs uni new rest' $ sequentExistsSubstitute exSub seq
    where (completeAtoms, rest) = Map.partition (all completeAtom) skMap
          func (a, ls)          = (\l -> (a, l)) <$> ls
          completeAtomsList     = concatMap func $ Map.toList completeAtoms
          completeAtom (FnRel _ ts) = not $ any isVariable (init ts)
          lookupElement atm = lookupElementInDB atm uni 
                              <|> lookupElementInDB atm new

{- As a helper for 'applyLoneSubs', returns the value corresponding to the 
   input atomic functional atom from a given database. -}
lookupElementInDB :: Atom -> Database -> Maybe (DB.Set Element)
lookupElementInDB (FnRel f ts) db = 
  case Map.lookup ref db of
    Nothing           -> Nothing -- table not found!
    Just set          ->
      let filterFunc = \(Tuple es _) ->
                         (Elem <$> (init $ Vect.toList es)) == init ts
                       -- filter records curresponding to the input atomic fact
          tups = DB.filter filterFunc set
      in  if   DB.null tups
          then Nothing -- term value not found
          else Just $  DB.map (\(Tuple es _) -> Vect.last es) tups
  where ref = case ts of
                [_] -> ConstTable (Constant f) -- unary function
                _   -> FnTable f

{- Another helper for 'applyLoneSubs' -}
transformTuples :: [(a, b)] -> ([a], [b])
transformTuples xs = (fst <$> xs, snd <$> xs)

{- Adds an 'Observation' to a 'Database'. -}
addToDatabase :: Observation -> Database -> Database
addToDatabase (Obs (Rel sym ts)) db = 
    let es  = (\(Elem e) -> e) <$> ts
        ref = RelTable sym
        tup = tuple $ Vect.fromList es
        tbl = Map.findWithDefault emptyTable ref db
    in  Map.insert ref (insertIntoTable tup tbl) db
addToDatabase (Obs (FnRel sym ts)) db = 
    let es  = (\(Elem e) -> e) <$> ts
        ref = FnTable sym
        tup = tuple $ Vect.fromList es
        tbl = Map.findWithDefault emptyTable ref db
    in  Map.insert ref (insertIntoTable tup tbl) db