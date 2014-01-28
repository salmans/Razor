{-| This moduel contains the basic operations regarding adding information to 
  a model (i.e., a database).
-}
module Chase.Problem.RelAlg.IOperations where

import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as State

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

import Chase.Problem.Provenance
import Chase.Problem.Observation
import Chase.Problem.RelAlg.RelAlg
import qualified RelAlg.DB as DB


{-| ProvCounter is the context of computation for building a model, including
  a state monad for a counter wrapped inside a state monad transformer for 
  provenance construction. Given an instance of pair (Prov, ProvInfo), namely 
  (prov, info), *prov* is the provenance for the tupls that are added to the 
  model during the current call to buildTables, and *info* is the provenance
  information for the model so far. We need *info* to rewrite elements that 
  happen to be true because of equality.
 -}
type ProvCounter = State.StateT (Prov, ProvInfo) Counter

{-| Updates a set of tables with a list a new equations. Given an empty set of 
  equations, this function returns the input set of tables, together with a
  set of tables containing the changes made to the input tables. -}
buildTables :: [Equation] -> Tables -> Tables -> ProvCounter (Tables, Tables)
buildTables eqs tbls delts = do
  (tbls', delts', eqs') <- buildTables' eqs tbls delts
  let ints              =  integrities tbls'
  if null ints
    then return (garbageCollect tbls', garbageCollect delts')
    else buildTables ints tbls' delts'
-- Salman: Delta can be another state monad

{- Garbage collection removes the intermediately generated skolem constants 
   from the model. -}
garbageCollect :: Tables -> Tables
garbageCollect tbls = 
    Map.filterWithKey filterFunc tbls
    where filterFunc = (\k _ -> 
                           case k of
                             ConTable ('a':'@': _) -> False
                             otherwise             -> True)


{- A helper for buildTables, which passes the input equations around in order
   to keep them in the reduced form with respect to the previously processed 
   equations. -}
buildTables' :: [Equation] -> Tables -> Tables -> 
                ProvCounter (Tables, Tables, [Equation])
buildTables' [] tbls  deltas       = return (tbls, deltas, [])
buildTables' (eq:eqs) tbls  deltas = do
    (tbls', deltas', eqs') <- processEquation eq (tbls, deltas, eqs)
    buildTables' eqs' tbls' deltas'

{- Processes a set of equations as a helper function to buildTables. -}
processEquation :: Equation -> (Tables, Tables, [Equation]) 
                -> ProvCounter (Tables, Tables, [Equation])
processEquation (Equ (Elm "True") (Elm "True")) _ = 
    error "CC.RelAlg.processEquation: invalid equation"
processEquation (Equ t (Elm "True")) (tbls, deltas, eqs) = do
  (tbls', deltas') <- insertRecord t tbls deltas  
  return (tbls', deltas', eqs)
          -- processing an equation that records a 
          -- relational fact.
processEquation (Equ (Elm "True") t) currentState = 
    processEquation (Equ t (Elm "True")) currentState
    -- orient the equation
processEquation (Equ c1@(Elm _) c2@(Elm _)) (tbls, deltas, eqs) = do
    (nt, tbls')  <- updateTables c1 c2 tbls
    (_, deltas') <- updateTables c1 c2 deltas
    let deltas'' =  mergeSets deltas' $ filterTables (elem nt) tbls'
    let eqs'     =  updateEquation c1 c2 <$> eqs
    return (tbls', deltas'', eqs')  -- making two constants equal in the 
                                    -- database.
processEquation (Equ t@(Fn f []) c@(Elm _)) (tbls, deltas, eqs) = do
  (recs, t')    <- initConstant tbls t
  (nt, tbls')   <- updateTables t' c (mergeSets tbls recs)
  (_, deltas')  <- updateTables t' c (mergeSets deltas recs)
  let deltas''  = mergeSets deltas' $ filterTables (elem nt) tbls'
  let eqs''     = updateEquation t' c <$> eqs
  return (tbls', deltas'', eqs'')
    -- making a new element in the database for a given constant.
    -- Salman: use state monad.        
processEquation (Equ c@(Elm _) t@(Fn f [])) inputState = 
    processEquation (Equ t c) inputState -- orient the equation    
processEquation (Equ t1@(Fn f1 []) t2@(Fn f2 [])) (tbls, deltas, eqs) = do
  (recs1, t1')  <- initConstant tbls t1
  (recs2, t2')  <- initConstant tbls t2
  (nt, tbls')   <- updateTables t1' t2' $ mergeAllSets [tbls,recs1,recs2]
  (_, deltas')  <- updateTables t1' t2' deltas
  -- Since the two constants are changing, add both of them to deltas:
  -- Salman: it should be enough to do this only for t2
  let t1Tbl     = Map.lookup (ConTable f1) tbls'
  let t2Tbl     = Map.lookup (ConTable f2) tbls'
  let deltas''  = mergeAllSets [deltas', recs1,recs2]
  let deltas''' = mergeSets deltas'' $ filterTables (elem nt) tbls'
  let eqs'      = updateEquation t1' t2' <$> eqs
  return (tbls', deltas''', eqs') 
    -- initiating two constants and make them equal.
          

processEquation _ _ = error "CC.RelAlg.processEquation: invalid equation"

{- Inserts a new record (corresponding to a term) to the database. Note that 
 we are treating relational facts as terms. -}
-- Salman: do not convert relational facts to terms since we don't have 
-- function symbols any more!
insertRecord :: Term -> Tables -> Tables -> ProvCounter (Tables, Tables)
insertRecord t tbls deltas = do
  let (sym, ref, ts) = case t of
                    (Rn s ts') -> (Rn s, RelTable s, ts')
                    (Fn s ts') -> (Fn s, FunTable s, ts')
  (rs, cs)     <- foldM (foldFunc) (tbls,[]) ts
  let r        =  Map.singleton ref (DB.Set [cs])
  let deltas'  =  mergeSets r rs
  let tbls'    =  mergeSets tbls deltas'
  let obs      =  termToObs True (sym cs)
  -- Adding new provenance information but first, convert the constants in the
  -- observation being logged are converted to the elements they are pointing:
  (prov, ProvInfo provs lastTag) <- State.get
  State.put (prov, ProvInfo (Map.insertWith (++) obs [prov] provs) lastTag)
  return (tbls', mergeSets deltas deltas')
  where foldFunc (rs, cs) t = do 
          (r', c') <- initConstant rs t
          return (mergeSetsWithKey unionFunc rs r', cs ++ [c'])
        unionFunc k = if k == DomTable then unionSets else const 
        -- Maintain the elements of the domain table but replace the elements
        -- of FunTables in order to assige every constant to only one value.
          
{- Given an input constant, returns the element in the model denoted by the 
   constant. If such an element does not exist, creates a new element and 
   returns a set of tables containing the inserted records for adding the new 
   element. -}
initConstant :: Tables -> Term -> ProvCounter (Tables, Term)
initConstant tbls t@(Fn s []) = do
  fresh    <- State.lift freshElement
  let recs = Map.fromList [(DomTable, DB.Set [[fresh]]),
                                   (ConTable s, DB.Set [[fresh]])]
  let (delts, finalVal) = case t' of
                           Just t'' -> (Map.empty, t'')
                           Nothing  -> (recs, fresh)

  let obs  = Eql t finalVal
  (prov, ProvInfo provs lastTag) <- State.get

  if "a@" `isPrefixOf` s
     then return (delts, finalVal)
     else do
       State.put ( prov
                 , ProvInfo (Map.insertWith (++) obs [prov] provs) lastTag)       
       return (delts, finalVal)
  -- This adds provenance for the constant whether the constant is being 
  -- initialized for the first time or it is just being referenced. Perhaps, 
  -- this is the desired behavior.
  where t'         = lookupConstant t tbls
          -- Salman: use counter monad.
          -- Salman: FunTable is in fact ConstTable
initConstant tbls t@(Elm _) = return (tbls, t)

{- Simply looks up a constant (or an element) in a set of tables and returns the
 element denoted by that constant (or element) if it exists. -}
lookupConstant :: Term -> Tables -> Maybe Term
lookupConstant (Fn c []) tbls =
    case Map.lookup (ConTable c) tbls of
      Nothing -> Nothing
      Just t  -> let set = DB.toList t
                 in  if   null set 
                     then Nothing
                     else if   null (head set)
                          then Nothing
                          else Just $ head $ head $ set
lookupConstant c@(Elm _) tbls =
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
updateTables :: Term -> Term -> Tables -> ProvCounter (Term, Tables)
updateTables c1@(Elm _) c2@(Elm _) tbls = do
  -- Also update provenance information for the elements being collapsed:
  (p, ps) <- State.get
  State.put (p, updateProvInfo c1 c2 ps)
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
updateTables _ _ _ = error "CC.RelAlg.updateTables: invalid update"    

updateProvInfo :: Term -> Term -> ProvInfo -> ProvInfo
updateProvInfo c1@(Elm _) c2@(Elm _) (ProvInfo info tag) = 
    ProvInfo ((updateProv <$>) <$> updateKeys info) tag
    where updateKeys = \m -> Map.fromListWith (++) -- union
                       $ (\(o, ss) -> (updateObs c1 c2 o, ss)) 
                             <$> (Map.toList m)
          updateProv = (\p -> 
                        case p of
                          ChaseProv tag id s -> 
                              ChaseProv tag id (updateSub s)
                          UserProv           -> UserProv)
                       -- Salman: Prov may instantiate Control.Applicative
          updateSub  = \s -> (Map.map updateFunc s)
          updateFunc = (\x -> if x == c1 then c2 else x)
updateProvInfo _ _ _ = error "CC.RelAlg.updateTables: invalid update"

updateEquation :: Term -> Term -> Equation -> Equation
updateEquation t1 t2 (Equ l r) = 
    Equ (updateTerm t1 t2 l) (updateTerm t1 t2 r)

updateObs :: Term -> Term -> Obs -> Obs
updateObs c1@(Elm _) c2@(Elm _) (Eql t1 t2) =
    Eql (updateTerm c1 c2 t1) (updateTerm c1 c2 t2)
updateObs c1@(Elm _) c2@(Elm _) (Fct (R sym ts)) =
    Fct (R sym (updateTerm c1 c2 <$> ts))
updateObs c1@(Elm _) c2@(Elm _) (Fct (F sym ts)) =
    Fct (F sym (updateTerm c1 c2 <$> ts))

updateTerm :: Term -> Term -> Term -> Term
updateTerm t1 t2 t@(Elm _)   = if t == t1 then t2 else t
updateTerm t1 t2 t@(Fn f ts) = 
    if   t == t1 
    then t2 
    else Fn f (updateTerm t1 t2 <$> ts)
updateTerm t1 t2 t@(Rn f ts) = 
    if   t == t1 
    then t2 
    else Rn f (updateTerm t1 t2 <$> ts)

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
integrities :: Tables -> [Equation]
integrities tbls = concatMap integrity sets
    where sets = Map.elems $ Map.filterWithKey (\k _ -> isFunTable k) tbls
          -- filter only function tables


isFunTable :: TableRef -> Bool
isFunTable (FunTable _) = True
isFunTable _            = False

{- Assuming that the input set is a FunTable, creates new equations to enforce
   integrity constraints over the function corresponding to the table. -}
integrity :: DB.Set [Term] -> [Equation]
integrity set = [Equ c1 c2 | ts1 <- list, ts2 <- list
                , let c1   =  last ts1
                , let c2   =  last ts2
                , init ts1 == init ts2
                , last ts1 /= last ts2]
    where list = DB.toList set