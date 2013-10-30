{-| This moduel contains the basic operations regarding adding information to 
  a model (i.e., a database).
-}
module Chase.Problem.RelAlg.IOperations where

import Control.Applicative
import Control.Monad

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace (trace)
import Control.Exception -- for assert
import Data.Tree


-- Logic modules:
import Formula.SyntaxGeo
import Utils.GeoUtilities
import Tools.GeoUnification

import Chase.Problem.RelAlg.RelAlg
import qualified RelAlg.DB as DB

{-| Updates a set of tables with a list a new equations. Given an empty set of 
  equations, this function returns the input set of tables, together with a
  set of tables containing the changes made to the input tables. -}
buildTables :: [Equation] -> Tables -> Counter (Tables, Tables)
buildTables eqs tbls = do
  (tbls', deltas', eqs') <- buildTables' eqs tbls emptyTables
  return (tbls', deltas')


{- A helper for buildTables, which passes the input equations around in order
   to keep them in the reduced form with respect to the previously processed 
   equations. -}
buildTables' :: [Equation] -> Tables -> Tables -> 
                Counter (Tables, Tables, [Equation])
buildTables' [] tbls  deltas       = return (tbls, deltas, [])
buildTables' (eq:eqs) tbls  deltas = do
    (tbls', deltas', eqs') <- processEquation eq (tbls, deltas, eqs)
    buildTables' eqs' tbls' deltas'

{- Processes a set of equations as a helper function to buildTables. -}
processEquation :: Equation -> (Tables, Tables, [Equation]) 
                -> Counter (Tables, Tables, [Equation])
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
processEquation (Equ c1@(Elm _) c2@(Elm _)) (tbls, deltas, eqs) = 
    return (tbls', deltas', eqs')  -- making two constants equal in the 
                                   -- database.
    where tbls'   = updateTables c1 c2 tbls
          deltas' = updateTables c1 c2 deltas
          eqs'    = updateEquation c1 c2 <$> eqs
processEquation (Equ t@(Fn f []) c@(Elm _)) (tbls, deltas, eqs) = do
  (recs, t')  <- initConstant tbls t
  let tbls'   =  mergeSets (updateTables t' c tbls) recs
  let deltas' =  mergeSets (updateTables t' c deltas) recs
  let eqs'    =  updateEquation t' c <$> eqs
  return (tbls', deltas', eqs')
    -- making a new element in the database for a given constant.
    -- Salman: use state monad.        
processEquation (Equ c@(Elm _) t@(Fn f [])) inputState = 
    processEquation (Equ t c) inputState -- orient the equation    
processEquation (Equ t1@(Fn f1 []) t2@(Fn f2 [])) (tbls, deltas, eqs) = do
  (recs1, t1') <- initConstant tbls t1
  (recs2, t2') <- initConstant tbls t2
  let tbls'    = updateTables t1' t2' $ mergeAllSets [tbls,recs1,recs2]
  let deltas'  = mergeAllSets [updateTables t1' t2' deltas,recs1,recs2]
  let eqs'     = updateEquation t1' t2' <$> eqs
  return (tbls', deltas', eqs') 
    -- initiating two constants and make them equal.
          

processEquation _ _ = error "CC.RelAlg.processEquation: invalid equation"

{- Inserts a new record (corresponding to a term) to the database. Note that 
 we are treating relational facts as terms. -}
-- Salman: do not convert relational facts to terms since we don't have 
-- function symbols any more!
insertRecord :: Term -> Tables -> Tables -> Counter (Tables, Tables)
insertRecord (Fn f ts) tbls deltas = do
  (rs, cs)     <- foldM (foldFunc) (tbls,[]) ts
  let r        =  Map.singleton (RelTable f) (DB.Set [cs])
  let deltas'  =  mergeSets r rs
  let tbls'    =  mergeSets tbls deltas'
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
initConstant :: Tables -> Term -> Counter (Tables, Term)
initConstant tbls t@(Fn s []) = do
  fresh    <- freshElement
  let recs = Map.fromList [(DomTable, DB.Set [[fresh]]),
                                   (FunTable s, DB.Set [[fresh]])]
  case t' of
    Just t'' -> return (Map.empty, t'')
    Nothing  -> return (recs, fresh)
  where t'         = lookupConstant t tbls
          -- Salman: use counter monad.
          -- Salman: FunTable is in fact ConstTable
initConstant tbls t@(Elm _) = return (tbls, t)

{- Simply looks up a constant (or an element) in a set of tables and returns the
 element denoted by that constant (or element) if it exists. -}
lookupConstant :: Term -> Tables -> Maybe Term
lookupConstant (Fn c []) tbls =
    case Map.lookup (FunTable c) tbls of
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
   every cell of every table of the database. -}
-- Salamn: this operation is probably the most constly operation. We may be 
-- able to reduce the cost by using references (pointers) or indices.
updateTables :: Term -> Term -> Tables -> Tables
updateTables c1@(Elm _) c2@(Elm _) tbls =
    nubSet.((updateFunction <$>) <$>) <$> tbls
    -- Salman: this can be done more efficiently
    where updateFunction = (\x -> if x == c1 then c2 else x) 
updateTables _ _ _ = error "CC.RelAlg.updateTables: invalid update"    

updateEquation :: Term -> Term -> Equation -> Equation
updateEquation t1 t2 (Equ l r) = 
    -- (trace.show) t1
    -- (trace.show) t2
    -- (trace.show) (Equ l r)
    Equ (updateTerm t1 t2 l) (updateTerm t1 t2 r)

updateTerm :: Term -> Term -> Term -> Term
updateTerm t1 t2 t@(Elm _)   = if t == t1 then t2 else t
updateTerm t1 t2 t@(Fn f ts) = 
    if   t == t1 
    then t2 
    else Fn f (updateTerm t1 t2 <$> ts)

-- -- Creates a fresh constant
-- freshConstant :: Int -> Term
-- freshConstant counter = Elm $ "const" ++ (show counter)