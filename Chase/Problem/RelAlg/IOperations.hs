{-| This moduel contains the basic operations regarding adding information to 
  a model (i.e., a database).
-}
module Chase.Problem.RelAlg.IOperations
    (lookupConstant,
     buildTables) where

import Control.Applicative


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
buildTables :: [Equation] -> Tables -> (Tables, Tables)
buildTables eqs tbls = 
    foldr processEquation (tbls, emptyTables) eqs

{- Processes a set of equations as a helper function to buildTables. -}
processEquation :: Equation -> (Tables, Tables) -> (Tables, Tables)
processEquation (Equ (Elm "True") (Elm "True")) _ = 
    error "CC.RelAlg.processEquation: invalid equation"
processEquation (Equ (Elm "True") t) (tbls, new) = 
    insertRecord t tbls new  -- processing an equation that records a 
                             -- relational fact.
processEquation (Equ t (Elm "True")) (tbls, new) = 
    insertRecord t tbls new
processEquation (Equ c1@(Elm _) c2@(Elm _)) (tbls, new) = 
    (updateTables c1 c2 tbls, new)  -- making two constants equal in the 
                                    --database.
processEquation (Equ t@(Fn f []) c@(Elm _)) (tbls, new) = 
    (tbls', new')  -- making a new element in the database for a given constant.
    where (recs, t') = initConstant tbls t
          tbls'      = mergeSets (updateTables t' c tbls) recs
          new'       = mergeSets new recs
processEquation (Equ c@(Elm _) t@(Fn f [])) (tbls, new) = 
    (tbls', new')
    where (recs, t') = initConstant tbls t
          tbls'      = mergeSets (updateTables t' c tbls) recs
          new'       = mergeSets new recs
          -- Salman: use counter monad.
          -- Salman: use state monad.
processEquation (Equ t1@(Fn f1 []) t2@(Fn f2 [])) (tbls, new) = 
    (tbls', new') -- initiating two constants and make them equal.
    where (recs1, t1') = initConstant tbls t1
          (recs2, t2') = initConstant tbls t2
          tbls'        = updateTables t1' t2' 
                         $ mergeAllSets [tbls,recs1,recs2]
          new'         = mergeAllSets [new,recs1,recs2]

processEquation _ _ = error "CC.RelAlg.processEquation: invalid equation"

{- Inserts a new record (corresponding to a term) to the database. Note that 
 we are treating relational facts as terms. -}
-- Salman: do not convert relational facts to terms since we don't have 
-- function symbols any more!
insertRecord :: Term -> Tables -> Tables -> (Tables, Tables)
insertRecord (Fn f ts) tbls new = 
    (tbls', mergeSets new new')
    where (rs, cs) = unzip $ initConstant tbls <$> ts
          r        = Map.singleton (RelTable f) (DB.Set [cs])
          tbls'    = mergeSets tbls new'
          new'     = mergeAllSets (r:rs)
{- Given an input constant, returns the element in the model denoted by the 
   constant. If such an element does not exist, creates a new element and 
   returns a set of tables containing the inserted records for adding the new 
   element. -}
initConstant :: Tables -> Term -> (Tables, Term)
initConstant tbls t@(Fn s []) = 
    case t' of
      Just t'' -> (Map.empty, t'')
      Nothing  -> (recs, fresh)
    where t'    = lookupConstant t tbls
          recs  = Map.fromList [(DomTable, DB.Set [[fresh]]),
                                (FunTable s, DB.Set [[fresh]])]
          fresh = freshConstant index
          index = length $ DB.toList $ 
                  Map.findWithDefault (DB.Set []) 
                         (DomTable) tbls
          -- Salman: use counter monad.
          -- Salman: FunTable is in fact ConstTable
initConstant tbls t@(Elm _) = (tbls, t)

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
    tbls'
    -- Map.adjust (\t -> (delete c1) <$> t) ("*", DomTable) tbls'
    -- Salman: why this is not working?!
    where updateFunction = (\x -> if x == c1 then c2 else x) 
          tbls'          = (((updateFunction <$>) <$>) <$>) tbls
          -- Salman: can you write it more efficiently using alter and update?!
updateTables _ _ _ = error "CC.RelAlg.updateTables: invalid update"    

-- Creates a fresh constant
freshConstant :: Int -> Term
freshConstant counter = Elm $ "const" ++ (show counter)