{-| This moduel contains the basic operations regarding adding information to 
  a model (i.e., a database).
-}
module Chase.Problem.RelAlg.IOperations where

import Control.Applicative
import Control.Monad
import qualified Control.Monad.State.Lazy as State
import qualified Control.Monad.List as List

import Data.List
import qualified Data.Map as Map
import Data.Maybe


import Debug.Trace
import Utils.Trace

import Control.Exception -- for assert


-- Logic modules:
import Formula.SyntaxGeo
import Utils.GeoUtilities
import Tools.GeoUnification
import Tools.Counter

import Chase.Problem.BaseTypes
import Chase.Problem.RelAlg.RelAlg
import qualified RelAlg.DB as DB


{-| ModelBuilder is the context of computation for building a model, including a 
  counter monad (for constructing fresh elements) wrapped inside a StateT for 
  element history construction. The result is then wrappend in a state monad for 
  provenance construction. Another state monad for that keeps track of 
  the changes in the initial model (delta of the database) forms another layer 
  of the monadic stack. Finally, a state monad that keeps track of the state
  of computation (observations left to process) sits at the top of the monadic 
  stack.

  The tuple (Term, Int, ElemHistory), e.g. (symbol, depth, history), contains 
  the skolem *symbol*, representing an existential quantifier which may create 
  new elements during the operations, the *depth* at which the histories (skolem 
  terms) of the collapsing elements must agree, and the history of all of the 
  previously created elements *history*.

  Given an instance of pair (Prov, ProvInfo), namely (prov, info), *prov* is the 
  provenance for the tupls that are added to the model during the current call 
  to buildTables, and *info* is the provenance information for the model so far.
  We need *info* to rewrite elements that happen to be true because of equality.
  
  The state of the last monad transformer is an instance of Tables, which stores 
  the changes in the database in the current computational context.

  [Obs] is the set of remaining observations to be added to the model.
  
  Finally, a list monad sits at the top of monadic stack for various 
  possibliites of instantiating skolem constants (a@xx) when collapsing is 
  enabled.
 -}
type Cntr             = CounterT []
type HistCntr         = (State.StateT (Maybe Term, Int, ElemHistory) Cntr)
type ProvHistCntr     = State.StateT (Prov, ProvInfo) HistCntr
type DeltProvHistCntr = State.StateT Tables ProvHistCntr
type ModelBuilder     = State.StateT [Obs] DeltProvHistCntr

liftObs     = id -- actually, we do not need to lift anything!
liftDelta   = liftObs.State.lift
liftProv    = liftDelta.State.lift
liftHist    = liftProv.State.lift
liftCounter = liftHist.State.lift
liftList    = liftCounter.List.lift


{-| Updates a set of tables with a set of observations passed in the context. 
  If the list of remaining observations is empty, buildTables returns the input 
  set of tables. Otherwise, it returns a new set of tables containing the new
  observations. The changes to the set of tables is implicitely computed inside 
  the monadic stack.
  After processing all observations (using buildTable' as a helper) buildTables
  calls the integrities function to obtain new observations that are necessary
  to mintain the integrity of function symbols in the database. The integrity
  observations will be processed in a recursive call (again, to buildTables) 
  until the database reaches a stable state, where no more integrity 
  observations are returned by integrities. -}
buildTables :: Tables -> ModelBuilder Tables
buildTables tbls = do
  tbls'     <- buildTables' tbls
  let obs   =  integrities tbls'
  if null obs
    then do -- Nothing to do more, prepare to return the results
      liftDelta $ State.modify garbageCollect
      return $ garbageCollect tbls'
    else do
      liftObs $ State.modify (++ obs)
      buildTables tbls'

{- A helper for buildTables, responsible to process *one* observation at a time. 
   buildTables' keeps calling itself recursively until no more observations are
   let to process. -}
buildTables' :: Tables ->  ModelBuilder Tables
buildTables' tbls  = do
  allObs <- liftObs $ State.get
  if null allObs 
     then return tbls
     else do
       ob:obs <- liftObs $ State.get
       liftObs $ State.put obs
       tbls'  <- processObs ob tbls
       buildTables' tbls'

{- Garbage collection removes the intermediately generated skolem constants 
   from the model. -}
garbageCollect :: Tables -> Tables
garbageCollect tbls = 
    Map.filterWithKey ((filterFunc.).const) tbls
    where filterFunc (ConTable ('a':'@': _)) = False
          filterFunc _                       = True
          -- Salman: we won't need this function if we don't greate the skolem
          -- constants in the first place.

{- Process an observation, i.e., add the observation to the input set of 
   tables. -}
processObs :: Obs -> Tables -> ModelBuilder Tables
processObs (Fct (R s ts)) tbls = do
  tbls' <- insertRecord (RelTable s) ts tbls
           -- Every relational fact is inserted as a new record in the table
           -- associated to a relational symbol.
  return tbls'
processObs (Fct (F s ts)) tbls = do
  tbls' <- insertRecord (FunTable s) ts tbls
           -- Just like the previous because we treat functions symbols as 
           -- special relations (see integrities function)
  return tbls'
processObs (Eql (Elm c1) (Elm c2)) tbls = do
    (nt, tbls')  <- updateTables c1 c2 tbls
    -- deltas       <- liftDelta State.get
    -- (_, deltas') <- updateTables c1 c2 deltas
    -- let deltas'' =  mergeSets deltas' $ filterTables (elem nt) tbls'
    liftDelta $ State.put tbls' -- instead of deltas'' for now
    liftObs   $ State.modify $ map (updateObs c1 c2)
    return tbls'
processObs (Eql t@(Fn f []) (Elm c)) tbls = do
  -- deltas        <- liftDelta $ State.get
  (recs, t')    <- initConstant tbls t
  (nt, tbls')   <- updateTables t' c (mergeSets tbls recs)
  -- (_, deltas')  <- updateTables t' c (mergeSets deltas recs)
  -- let deltas''  = mergeSets deltas' $ filterTables (elem nt) tbls'
  liftDelta $ State.put tbls'
  liftObs   $ State.modify $ map (updateObs t' c)
  return tbls'
processObs (Eql c@(Elm _) t@(Fn f [])) tbls = processObs (Eql t c) tbls -- orient
processObs (Eql t1@(Fn f1 []) t2@(Fn f2 [])) tbls = do
  -- deltas        <- liftDelta State.get
  obs           <- liftObs   State.get
  (recs1, t1')  <- initConstant tbls t1
  (recs2, t2')  <- initConstant tbls t2
  (nt, tbls')   <- updateTables t1' t2' $ mergeAllSets [tbls,recs1,recs2]
  -- (_, deltas')  <- updateTables t1' t2' deltas
  -- Since the two constants are changing, add both of them to deltas:
  -- Salman: it should be enough to do this only for t2
  let t1Tbl     = Map.lookup (ConTable f1) tbls'
  let t2Tbl     = Map.lookup (ConTable f2) tbls'
  -- let deltas''  = mergeAllSets [deltas', recs1,recs2]
  -- let deltas''' = mergeSets deltas'' $ filterTables (elem nt) tbls'
  let obs'      = updateObs t1' t2' <$> obs

  liftDelta $ State.put tbls'
  liftObs   $ State.put obs'
  return tbls'
processObs _ _ = error "CC.RelAlg.processObs: invalid equation"

{- Inserts a new record (corresponding to the terms of a single tuple) to a 
   specific table of a database. -}
insertRecord :: TableRef -> [Term] -> Tables -> ModelBuilder Tables
insertRecord ref ts tbls = do
  (rs, es)     <- foldM insertFunc (tbls,[]) ts
                  -- Insert the tuples in the database: the terms in ts are
                  -- denoting the elements in es. rs are the changes in the 
                  -- database after adding ts to ref, containing the changes to
                  -- the domain table and the changes to constant tables.
  let r        =  Map.singleton ref (DB.Set [es])
                  -- r is the new record in the table denoted by ref
  let deltas   =  mergeSets r rs -- All changes to the database
  let tbls'    =  mergeSets tbls deltas -- Update the database

  -- Adding new provenance information but first, convert the constants in the
  -- observation being logged are converted to the elements they are pointing:
  let obs      =  atomToObs $ case ref of 
                                RelTable s -> R s ts
                                FunTable s -> F s ts
  (prov, ProvInfo provs lastTag) <- liftProv State.get
  let provInfo = ProvInfo (Map.insertWith (++) obs [prov] provs) lastTag

  liftProv $ State.put (prov, provInfo)
  -- Updating the changes to the database in the current context:
  liftDelta $ State.modify $ mergeSets deltas
  return tbls'
  where insertFunc (rs, es) t = do 
          (r', e') <- initConstant rs t
          return (mergeSetsWithKey unionFunc rs r', es ++ [e'])
        unionFunc k = if k == DomTable then unionSets else const 
        -- Maintain the elements of the domain table but replace the elements
        -- of FunTables in order to assige every constant to only one value.
          
{- Given an input constant, returns the element in the model denoted by the 
   constant. If the constant does not denote anything, creates a new element and 
   returns a set of tables containing the inserted records for adding the new 
   element. -}
initConstant :: Tables -> Term -> ModelBuilder (Tables, Elem)
initConstant tbls t@(Fn s []) = do
  let t' = lookupConstant t tbls

  if "a@" `isPrefixOf` s -- temporary skolem constant
     then do          
       (Just skTerm@(Fn f ts), depth, hist) <- liftHist State.get
       let ts' = (\(Elm term) -> fromJust (lookup term hist)) <$> ts
                 -- map ts to their histories (skolem terms)
                 -- NOTE: this line makes a couple of assumptions, which are
                 -- invariants of the program:
                 -- 1. Every element in ts has to be an element term (Elm t),
                 -- because we flatten the terms as a preprocessing step.
                 -- 2. Every element in ts must have a history in hist.
                 -- For now, if any of the following conditions fails, we get
                 -- a pattern matching error. A data-type refactoring can make
                 -- the code prettier!

       vs <- liftCounter $ getValue depth (Fn f ts') hist
       v  <- liftList [head vs]

       let recs = Map.fromList [(DomTable, DB.Set [[v]]),
                                (ConTable s, DB.Set [[v]])]
       let (delts, finalVal) = case t' of
                                 Just t'' -> (Map.empty, t'')
                                 Nothing  -> (recs, v)
                                 
       let obs  = Eql t (Elm finalVal)
       (prov, ProvInfo provs lastTag) <- liftProv State.get
       
       liftHist $ State.put $ (Just skTerm, depth, ((v, (Fn f ts')):hist))
       return (delts, finalVal)
     else do
       fresh <- liftCounter freshElementT
       (skTerm, depth, hist) <- liftHist State.get
       let recs = Map.fromList [(DomTable, DB.Set [[fresh]]),
                                (ConTable s, DB.Set [[fresh]])]
       let (delts, finalVal) = case t' of
                                 Just t'' -> (Map.empty, t'')
                                 Nothing  -> (recs, fresh)

       let obs  = Eql t (Elm finalVal)
       (prov, ProvInfo provs lastTag) <- liftProv State.get

       let provInfo = ProvInfo (Map.insertWith (++) obs [prov] provs) lastTag
       liftProv $ State.put (prov, provInfo)
       liftHist $ State.put $ (skTerm, depth, nub ((fresh, t):hist))
       return (delts, finalVal)
  -- This adds provenance for the constant whether the constant is being 
  -- initialized for the first time or it is just being referenced. Perhaps, 
  -- this is the desired behavior.
          -- Salman: use counter monad.
          -- Salman: FunTable is in fact ConstTable
initConstant tbls (Elm e) = return (tbls, e)

{- Simply looks up a constant (or an element) in a set of tables and returns the
 element denoted by that constant (or element) if it exists. -}
lookupConstant :: Term -> Tables -> Maybe Elem
lookupConstant (Fn c []) tbls =
    case Map.lookup (ConTable c) tbls of
      Nothing -> Nothing
      Just t  -> let set = DB.toList t
                 in  if   null set 
                     then Nothing
                     else if   null (head set)
                          then Nothing
                          else Just $ head $ head $ set
lookupConstant (Elm c) tbls =
    case Map.lookup DomTable tbls of
      Nothing -> Nothing
      Just t  -> let set = DB.toList t
                 in  if   [c] `elem` set 
                     then Just c
                     else Nothing
lookupConstant _ _ = error "CC.RelAlg.lookupConstant: invalid element!"

{- Given two elements, replaces the first element with the second element in 
   every cell of every table of the database. The funciton rewrites the greater
   term to the smaller term under the ordering of Term. The function also 
   returns the smaller term, which is the term it rewrites to. -}
-- Salamn: this operation is probably the most constly operation. We may be 
-- able to reduce the cost by using references (pointers) or indices.
updateTables :: Elem -> Elem -> Tables -> ModelBuilder (Elem, Tables)
updateTables c1 c2 tbls = do
  -- Also update provenance information for the elements being collapsed:
  (p, ps) <- liftProv State.get
  liftProv $ State.put (p, updateProvInfo c1 c2 ps)
  -- return $ (if n1 > n2 then c2 else c1, 
  --           nubSet.((updateFunc <$>) <$>) <$> tbls)
  return (c2, nubSet.((updateFunc <$>) <$>) <$> tbls)
    -- Salman: this can be done more efficiently
    where updateFunc = \x -> if x == c1 then c2 else x
              -- if   n1 > n2
              --          then (\x -> if x == c1 then c2 else x)
              --          else (\x -> if x == c2 then c1 else x)

  -- Salman: if you want to rewrite to the smallest element, you need to orient
  -- the remaining equations in the queue. Consider the following as a test 
  -- case:
  -- "exists x. exists y. exists z. R(x, y, z)"
  -- "R(x, y, z) => f(z) = x"
  -- "R(x, y, z) => f(z) = y"

updateProvInfo :: Elem -> Elem -> ProvInfo -> ProvInfo
updateProvInfo e1 e2 (ProvInfo info tag) = 
    ProvInfo ((updateProv <$>) <$> updateKeys info) tag
    where updateKeys = \m -> Map.fromListWith (++) -- union
                       $ (\(o, ss) -> (updateObs e1 e2 o, ss)) 
                             <$> (Map.toList m)
          updateProv = (\p -> 
                        case p of
                          ChaseProv tag id s -> 
                              ChaseProv tag id (updateSub s)
                          UserProv           -> UserProv)
                       -- Salman: Prov may instantiate Control.Applicative
          updateSub  = \s -> (Map.map updateFunc s)
          updateFunc = (\x -> updateTerm e1 e2 x)

updateObs :: Elem -> Elem -> Obs -> Obs
updateObs e1 e2 (Eql t1 t2) =
    Eql (updateTerm e1 e2 t1) (updateTerm e1 e2 t2)
updateObs e1 e2 (Fct (R sym ts)) =
    Fct (R sym (updateTerm e1 e2 <$> ts))
updateObs e1 e2 (Fct (F sym ts)) =
    Fct (F sym (updateTerm e1 e2 <$> ts))

updateTerm :: Elem -> Elem -> Term -> Term
updateTerm e1 e2 (Elm e)   = if e == e1 then Elm e2 else Elm e
updateTerm e1 e2 t@(Fn f ts) = Fn f (updateTerm e1 e2 <$> ts)
updateTerm e1 e2 t@(Rn f ts) = Rn f (updateTerm e1 e2 <$> ts)

filterTables :: (Record -> Bool) -> Tables -> Tables
filterTables f tbls =  
    Map.foldWithKey foldFunc emptyTables tbls
    where foldFunc k tbl acc = 
              let filteredTbl = filterTable f tbl
              in  if (null.DB.toList) filteredTbl
                  then acc
                  else Map.insert k filteredTbl acc

filterTable :: (Record -> Bool) -> Table -> Table
filterTable f (DB.Set tbl) = DB.Set (filter f tbl)

{- Returns all the equations that enforce integrity constraints in the current
   state of the model. -}
integrities :: Tables -> [Obs]
integrities tbls = concatMap integrity sets
    where sets = Map.elems $ Map.filterWithKey (\k _ -> isFunTable k) tbls
          -- filter only function tables


isFunTable :: TableRef -> Bool
isFunTable (FunTable _) = True
isFunTable _            = False

{- Assuming that the input set is a FunTable, creates new equations to enforce
   integrity constraints over the function corresponding to the table. -}
integrity :: DB.Set [Elem] -> [Obs]
integrity set = [Eql (Elm c1) (Elm c2) | ts1 <- list, ts2 <- list
                , let c1   =  last ts1
                , let c2   =  last ts2
                , init ts1 == init ts2
                , last ts1 /= last ts2]
    where list = DB.toList set

-- Collapsec functions
getValue :: Int -> SkolemTerm -> ElemHistory -> Cntr [Elem]
getValue depth skTerm hist = 
    let vals = selectCollapses depth skTerm hist
    in  if   null vals
        then do
          fresh <- freshElementT
          return [fresh]
        else return vals

selectCollapses :: Int -> SkolemTerm -> ElemHistory -> [Elem]
selectCollapses (-1) _ _          = [] -- Do not collapse anything
selectCollapses depth skTerm hist =
    nub $ fst <$> filter ((matchSkolemTerms depth skTerm).snd) hist

matchSkolemTerms :: Int -> SkolemTerm -> SkolemTerm -> Bool
matchSkolemTerms 0 _ _                         = True
matchSkolemTerms _ (Elm e1) (Elm e2)           = e1 == e2
matchSkolemTerms depth (Fn f1 ts1) (Fn f2 ts2) = 
    f1 == f2 && (and $ zipWith (matchSkolemTerms (depth - 1)) ts1 ts2)
matchSkolemTerms _ t1 t2                       = False