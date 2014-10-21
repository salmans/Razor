{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DoAndIfThenElse #-}


{- Razor
   Module      : Chase.HerbrandBase.RelAlg.ITranslate
   Description : Implements the translating functions from geometric logic to
   relational algebra.
   Maintainer  : Salman Saghafi -}

module Chase.HerbrandBase.RelAlg.ITranslate where

-- Standard
import Data.Tuple (swap)
import Data.List (elemIndex, find, maximumBy, nub, (\\))
import Data.Either
import qualified Data.Vector as Vect
import Data.Vector ((!), cons)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, fromMaybe)

-- Control
import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as State

-- Syntax
import Syntax.GeometricUtils 
    ( Variable (..), Constant (..), Term (..), Element (..)
    , FnSym, termDepth -- Term
    , ExistsSub (..), emptyExistsSub -- Substitution
    , Atom (..), Formula (..) -- Geometric
    , termToVariable, termToConstant, freeVars
    , freshElement)

-- Common
import Common.Basic (Id)
import Common.Observation (Observation (..))
import Common.Provenance ( ProvInfo (..), Blame (..)
                         , addElementProv, modifyElementProvs, getElementProv )

-- Chase
import Chase.Data ( PushM, PullM, liftPushMBase, liftPushMProvs
                  , liftPushMCounter, liftPullMProvs )
import Chase.HerbrandBase.RelAlg.Lang -- import everything
import qualified Chase.HerbrandBase.RelAlg.DB as DB
import qualified Tools.ExtendedSet as ExSet

-- Error Messages:
unitName               = "Chase.HerbrandSet.RelAlg.Translate"
error_noFunctionSymbol = "function symbols are not permitted"
error_noElement        = "elements are not permitted- " 
                         ++ "terms must be over the original vocabulary"
error_projIndTooLarge  = "index for projecting the table is too large"
error_insertIntoDeltaView   = "cannot insert into a delta view"
error_insertIntoUnionView   = "cannot insert into a delta view"
error_invalidRelExp    = "relational expression is invalid"
error_headersMismatch  = "the header of the joined expression and one of its"
                         ++ " components do not match"
error_freeVarInHead    = "the expression in head has extra attributes"
error_notConstTable    = "expecting a reference to a constant table"
error_deltaForDelta    = "the formula is already in differential form"
--------------------------------------------------------------------------------
-- Translating sequents to relational algebra:

{- Creates a 'RelExp' for a 'Formula' in the head of a sequent. Unlike the 
   formulas in body of sequents, the head formulas may have disjunctions at 
   their topmost level. -}
headRelExp :: Formula -> [RelExp]
headRelExp (Or fmla1 fmla2) = 
    if   fmla1' == Tru || fmla2' == Tru
    then headRelExp Tru  -- shortcut Truth
    else (headRelExp fmla1') ++ (headRelExp fmla2')
    where fmla1' = removeHeadEquations fmla1
          fmla2' = removeHeadEquations fmla2
headRelExp fmla             = [formulaRelExp fmla']
    where fmla'  = removeHeadEquations fmla

{- Creates a 'RelExp' for a 'Formula' in the body of a sequent. -}
bodyRelExp :: Formula -> RelExp
bodyRelExp = (formulaRelExp . removeBodyEquations)

{- Replaces the equations in the body of a sequent with Truth. This is 
   necessary for constructing the correct set of observational instances that
   are passed to the SMT solver. -}
removeBodyEquations :: Formula -> Formula
removeBodyEquations fmla =
    let fmla' = removeBodyEquationsHelper fmla
        diff  = (freeVars fmla) \\ (freeVars fmla')
    in  case (fmla', diff) of
          (f  , []) -> f
          (Tru, vs) -> let rels = (\v -> Atm $ Rel "@Element" [Var v]) <$> vs
                       in foldr1 And rels
          (f  , vs) -> let rels = (\v -> Atm $ Rel "@Element" [Var v]) <$> vs
                       in foldr And f rels

removeBodyEquationsHelper :: Formula -> Formula
removeBodyEquationsHelper Tru = Tru
removeBodyEquationsHelper (Atm (Rel "=" _))  = Tru
removeBodyEquationsHelper a@(Atm _)          = a
removeBodyEquationsHelper (And fmla1 fmla2)  =
    case (removeBodyEquations fmla1, removeBodyEquations fmla2) of
      (Tru, Tru) -> Tru
      (Tru, f  ) -> f
      (f  , Tru) -> f
      (f  , f' ) -> And f f'
removeBodyEquationsHelper (Lone sk x fmla unq) = 
    Lone sk x (removeBodyEquationsHelper fmla) unq

{- Replaces the equations in the head of a sequent with Falsehood. -}
removeHeadEquations :: Formula -> Formula
removeHeadEquations Tru = Tru
removeHeadEquations Fls = Fls
removeHeadEquations (Atm (Rel "=" _))  = Tru
removeHeadEquations a@(Atm _)          = a
removeHeadEquations (And fmla1 fmla2)  =
    case (removeHeadEquations fmla1, removeHeadEquations fmla2) of
      (Fls, _  ) -> Fls
      (_  , Fls) -> Fls
      (Tru, f  ) -> f
      (f  ,Tru ) -> f
      (f  , f' ) -> And f f'
removeHeadEquations (Or fmla1 fmla2)    =
    case (removeHeadEquations fmla1, removeHeadEquations fmla2) of
      (Fls, f  ) -> f
      (f  , Fls) -> f
      (f  , f' ) -> Or f f'
removeHeadEquations (Exists fn x fmla)  = 
    Exists fn x (removeHeadEquations fmla)
removeHeadEquations (Lone sk x fmla unq) =
    Lone sk x (removeHeadEquations fmla) unq

{- Translates a disjunct-free goemetric 'Formula' to a 'RelExp'. -}
formulaRelExp :: Formula -> RelExp
formulaRelExp Fls                 = TblEmpty
formulaRelExp Tru                 = TblFull
formulaRelExp (Atm a)             = atomRelExp a
formulaRelExp (And fmla1 fmla2)   =
    if (fmla1 == Fls || fmla2 == Fls) 
    then formulaRelExp Fls -- shortcut Falsehood
    else joinRelExp res1 res2 -- Join
    where res1 = formulaRelExp fmla1  -- apply recursion
          res2 = formulaRelExp fmla2
formulaRelExp (Exists fn x fmla)   =
    case formulaRelExp fmla of
      TblFull   -> TblFull
      TblEmpty  -> TblEmpty
      exp       -> case Map.lookup x (header exp) of
                     Nothing -> exp -- existential variable is redundant
                     Just i  -> let newHeader  = Map.delete x (header exp)
                                    newHeader' = shift i newHeader
                                in  Proj exp i newHeader' (fromJust fn) Nothing
                                    -- Assumes that the exitential quantifier
                                    -- has been assigned to a skolem function,
                                    -- at least, in the preprocessing phase.
    where shift i m = Map.map (\x -> if x > i then x - 1 else x) m
          -- a little helper that shifts the columns after the projected column
          -- to left
formulaRelExp (Lone fn x fmla lfmla)   =
    case formulaRelExp fmla of
      TblFull   -> TblFull
      TblEmpty  -> TblEmpty
      exp       -> case Map.lookup x (header exp) of
                     Nothing -> exp -- existential variable is redundant
                     Just i  -> let newHeader  = Map.delete x (header exp)
                                    newHeader' = shift i newHeader
                                in  Proj exp i newHeader' (fromJust fn) 
                                        (Just lExp)
                                    -- Assumes that the exitential quantifier
                                    -- has been assigned to a skolem function,
                                    -- at least, in the preprocessing phase.
    where shift i m = Map.map (\x -> if x > i then x - 1 else x) m
          -- a little helper that shifts the columns after the projected column
          -- to left
          lExp      = formulaRelExp lfmla

{- Creates a join expression for two input instances of 'RelExp' and their
   'Schema' instances. -}
joinRelExp :: RelExp -> RelExp -> RelExp
joinRelExp lExp rExp 
    | lExp == TblEmpty || rExp == TblEmpty = TblEmpty
    | lExp == TblFull                      = rExp
    | rExp == TblFull                      = lExp
    | otherwise                            = 
        let lHeader    = header lExp
            rHeader    = header rExp
            newHeader  = joinHeader lHeader rHeader
        in Join lExp rExp newHeader

{- As a helper for joinRelExp, computes the header of a join expression for two
   expressions with the two input headers -}
joinHeader :: Header -> Header -> Header
joinHeader lHeader rHeader =
    let rHeader'  = Map.difference rHeader lHeader
                    -- items that only exist in rHeader
        lHeaderSize = Map.size lHeader
        lHeader''   = Map.fromList $ zip (Map.keys lHeader) [0..]
        rHeader''   = Map.fromList $ zip (Map.keys rHeader') [lHeaderSize..]
                      -- shift the elements in the left and the right 
                      -- header, ignore the columns with no 
                      -- variable attributes
    in Map.unionWith const lHeader'' rHeader''

{- Creates a list of pairs, representing a join predicate on two input 
   'Header's. The first input is the header of the new expression 
   (join expression) and the second parameter is the header of one of the two
   expressions being joined. -}
varJoinPairs :: Header -> Header -> [(Int, Int)]
varJoinPairs newHeader oldHeader = 
    if   Map.null newHeader || Map.null oldHeader
    then []
    else Map.foldrWithKey foldPred [] oldHeader
         -- reverse makes sure the mapping is made in order
    where foldPred k ps pairs = (pairsOf k ps):pairs
          pairsOf k pos       = 
              case Map.lookup k newHeader of
                Nothing     -> error $ unitName ++ ".varJoinPairs: " ++ 
                               error_headersMismatch
                Just newInd -> (pos, newInd)
--    Map.elems $ Map.intersectionWith (,) lHeader rHeader

{- Builds a 'RelExp' for a given atomic formula. -}
atomRelExp :: Atom -> RelExp
atomRelExp (FnRel sym [e]) = atomTable (ConstTable (Constant sym)) [e]
atomRelExp (FnRel sym ts)  = atomTable (FnTable  sym) ts
atomRelExp (Rel   sym ts)  = atomTable (RelTable sym) ts

{- A helper for atomRelExp that builds an instance of 'RelExp' for an atomic
   formula, considering the constants and variables in the atom. -}
atomTable :: TableRef -> [Term] -> RelExp
atomTable ref ts = 
    let intMap  = createInternalVarMap ts
        heads   = createExternalVarMap ts
        eqPairs = equalVarPairs intMap heads
        vars    = [(i, j) | (i, t) <- (zip [0..] ts) 
                  , let v    = termToVariable t
                  , let j    = fromJust $ Map.lookup (fromJust v) heads
                  , isJust v ]
        exp     = Tbl ref vars heads
    in  if   (length eqPairs) == (length $ Map.keys heads)
        -- none of the variables are mapped twice- select every column
        then exp
        else Sel exp eqPairs heads


{- As a helper for 'atomTable', constructs a set of pairs to filter the tuples 
   of a set based on the positions of an atomic formula's terms that share the 
   same variables.
-}
equalVarPairs :: Map.Map Variable [Int] -> Header -> [(Int, Int)]
equalVarPairs varMap heads = 
    Map.foldWithKey foldPred [] varMap
    where foldPred k ps pairs = (pairsOf k ps) ++  pairs
          pairsOf k pos       = 
              let newInd = fromJust $ Map.lookup k heads
              in  (\t -> (t, newInd)) <$> pos


{-

{- As a helper for 'atomTable', constructs a set of pairs to filter the tuples 
   of a set based on the elements appearing in the terms of an atomic formula. -}
elemPreds :: [Term] -> [(Int, Element)]
elemPreds ts = fst $ foldl foldFunc ([], 0) ts
    where foldFunc (pairs, i) t = 
              case t of
                Elem e    -> ((i, e): pairs, i + 1)                
                otherwise -> (pairs, i + 1)  -- otherwise, ignore

{- Returns a 'Schema' for a given list of terms. -}
termSchema :: [Term] -> Schema
termSchema ts = labelingFunc <$> ts
    where labelingFunc t = case t of 
                             Var v     -> Just v
                             otherwise -> Nothing
-}

{- Creates a map from variables to their positions in a list of terms.-}
createExternalVarMap :: [Term] -> Map.Map Variable Int
createExternalVarMap ts = let vs       = [ fromJust v | t <- ts
                                         , let v = termToVariable t, isJust v]
                              varPairs = zip (nub vs) [0..]
                          in  Map.fromList varPairs
                      -- Using flip to make sure that the indices are created
                      -- in the right order

createInternalVarMap :: [Term] -> Map.Map Variable [Int]     
createInternalVarMap ts = let termPairs = zip ts [0..]
                              varPairs  = 
                                  [(fromJust  v, [i]) | (t, i) <- termPairs
                                  , let v = termToVariable t
                                  , isJust v ]                      
                          in  Map.fromListWith (flip (++)) varPairs
                      -- Using flip to make sure that the indices are created
                      -- in the right order

{- Computes a differential relational expression for an input relational 
  expression. The differential expression is used for incremental view 
  maintenance. -}
delta :: RelExp -> RelExp
delta TblEmpty               = TblEmpty
delta TblFull                = TblFull -- This assumes sequents with TblFull on
  -- their left are processed once at the beginning and will be never processed
  -- again.
delta exp@(Tbl _ _ hds)         = Delta exp hds
delta (Proj exp cols hds fn lf) = Proj (delta exp) cols hds fn lf
delta (Sel exp cols hds)        = Sel (delta exp) cols hds
delta (Join exp1 exp2 hds)      = Union exp1 dlt1 exp2 dlt2 hds
    where dlt1 = delta exp1
          dlt2 = delta exp2
delta _                      = error $ unitName ++ ".delta: " ++ 
                               error_deltaForDelta

--------------------------------------------------------------------------------
-- Evaluating sequents in a database:

{-| Evaluates a relational expression in a set of 'Database' as the primary
   database and a 'Database' corresponding to the changes in the database. 

   Inputs:
   - current database
   - the last set of changes to the database
   - input relational expression
   
   Output:
   - An instance of 'ExtendedTable' with 'ExistsSub' as extra information for
   each tuple. The substitutions are used to determine the mapping that is used
   to instantiate projected columns (existential variables) when computing the
   tuple.
-}
{- Future extension: as a safety constraint, the header of the expression may be
   compared against the schema for the table being fetched from the database -}
evaluateRelExp :: Database -> Database -> RelExp -> TableSub
evaluateRelExp _ _ TblEmpty    = emptyTableSub
evaluateRelExp _ _ TblFull     = fullTableSub
evaluateRelExp db dlt exp@(Tbl t _ heads) = 
    let set = Map.findWithDefault emptyTable t db
    in  decorateTable set emptyExistsSub
evaluateRelExp db dlt (Delta exp@(Tbl t _ heads) _) =
    let set = Map.findWithDefault emptyTable t dlt
    in  decorateTable set emptyExistsSub
evaluateRelExp tbls delts (Proj exp col _ fn _) =
  let set  = evaluateRelExp tbls delts exp                                  
  in if   nullTableSub set
     then evaluateRelExp tbls delts TblEmpty
       -- if the non-projected set is empty, evaluate @TblEmpty@
     else DB.project proj set
    where proj = DB.Project $
                 \tup@(Tuple vec inf) -> 
                     let (DB.Project pf) = deleteColumnProjector col
                     -- First, create a DB.Project instance that gets rid
                     -- of the extra column.
                         inf'            = Map.insert fn (Elem (vec ! col)) inf
                         -- Update the ExistsSub information as above
                     in  pf tup { tupleDec = inf' }
    -- where getProv ps e = getElementProv e ps
    --       separate ls  = let (h, t) = Vect.splitAt (col + 1) ls
    --                      in  ( Vect.last h
    --                          , fromJust <$> Vect.toList ((Vect.init h) Vect.++ t))

evaluateRelExp tbls delts (Sel exp pairs _) =
    let set    = evaluateRelExp tbls delts exp
        selFun = similarColumnsSelector pairs
    in  DB.select selFun set
    -- Select columns that are equal according to @pairs@
evaluateRelExp tbls delts (Join lExp rExp heads) =
    let lHds  = header lExp
        rHds  = header rExp
        lSet  = evaluateRelExp tbls delts lExp
        rSet  = evaluateRelExp tbls delts rExp
        ps    = Map.elems $ Map.intersectionWith (,) lHds rHds
        cond  = DB.Select $ \(Tuple x _ , Tuple y _) -> 
                and $ (\(p1, p2) -> x ! p1 == y ! p2) <$> ps
                -- expanding variable pairs as a filter function for DB.join
        tran  = \t1 t2 -> joinTupleTransformer lHds rHds heads t1 t2
    in  DB.Set $ map (uncurry tran) 
               $ DB.contents (DB.join cond lSet rSet)

evaluateRelExp tbls delts (Union lExp lDlt rExp rDlt heads) =
    let lHds = header lExp
        rHds = header rExp
        ps   = Map.elems $ Map.intersectionWith (,) lHds rHds
        cond = DB.Select $ \(Tuple x _, Tuple y _) -> 
               and $ (\(p1, p2) -> x ! p1 == y ! p2) <$> ps
        tran = \t1 t2 -> joinTupleTransformer lHds rHds heads t1 t2
        set1 = DB.Set $ map (uncurry tran) 
               $ DB.contents (DB.join cond slExp srDlt)
        set2 = DB.Set $ map (uncurry tran) 
               $ DB.contents (DB.join cond slDlt srExp)
        set3 = DB.Set $ map (uncurry tran) 
               $ DB.contents (DB.join cond slDlt srDlt)
    in  unionTables (unionTables set1 set2) set3
    where slExp = evaluateRelExp tbls delts lExp
          srExp = evaluateRelExp tbls delts rExp
          slDlt = evaluateRelExp tbls delts lDlt
          srDlt = evaluateRelExp tbls delts rDlt

{- Evaluates a relational expression in a single database. Unlike 
   'evaluateRelExp', this function does not compute delta expressions 
   (if there is any 'Union' or 'Delta') in the expression. -}
evaluateRelExpNoDelta :: Database -> RelExp -> TableSub
evaluateRelExpNoDelta _ TblEmpty    = emptyTableSub
evaluateRelExpNoDelta _ TblFull     = fullTableSub
evaluateRelExpNoDelta db exp@(Tbl _ _ _) = 
    evaluateRelExp db emptyDatabase exp
evaluateRelExpNoDelta db (Proj exp col _ fn _) =
    let set       = evaluateRelExpNoDelta db exp

        projFunc  = DB.Project $
                    \tup@(Tuple vec inf) -> 
                    let (DB.Project pf) = deleteColumnProjector col
                        inf'            = Map.insert fn (Elem (vec ! col)) inf
                    in  pf tup { tupleDec = inf' }
        projected = DB.project projFunc set
    in  if   nullTableSub set
        then evaluateRelExpNoDelta db TblEmpty
             -- if the non-projected set is empty, evaluate @TblEmpty@
        else projected
    -- where getProv ps e = getElementProv e ps
    --       separate ls  = let (h, t) = Vect.splitAt (col + 1) ls
    --                      in  ( Vect.last h
    --                          , fromJust <$> (Vect.toList ((Vect.init h) Vect.++ t)))

evaluateRelExpNoDelta db (Sel exp pairs _) =
    let set    = evaluateRelExpNoDelta db exp
        selFun = similarColumnsSelector pairs
    in  DB.select selFun set
    -- Select columns that are equal according to @pairs@
evaluateRelExpNoDelta db (Join lExp rExp heads) =
    let lHds  = header lExp
        rHds  = header rExp
        lSet  = evaluateRelExpNoDelta db lExp
        rSet  = evaluateRelExpNoDelta db rExp
        ps    = Map.elems $ Map.intersectionWith (,) lHds rHds
        cond  = DB.Select $ \(Tuple x _, Tuple y _) -> 
                and $ (\(p1, p2) -> x ! p1 == y ! p2) <$> ps
                -- expanding variable pairs as a filter function for DB.join
        tran  = \t1 t2 -> joinTupleTransformer lHds rHds heads t1 t2
    in  DB.Set $ map (uncurry tran)
               $ DB.contents (DB.join cond lSet rSet)
-- mergeJoinTables (DB.join cond lSet rSet) $ snd <$> ps
--------------------------------------------------------------------------------
-- Inserting tuples for relational expressions:

{- Inserts the tuples of a 'Table' @t@ to a view for relational expression @e@
   in a database @db@. The schema for @t@ must be compatible with the @e@, 
   otherwise, the function terminates with an error. 
   
   NOTE: in the current implementation, this function does not allow for
   duplicate entries: if a table contains a tuple being inserted, the function 
   will keep only once instance of the tuple. 
   
   NOTE: the function assumes that the expression does not have any reference
   to a delta 'Database' (unlike evaluateRelExp). -}

insertTuples :: TablePair -> RelExp -> Database -> Int 
              -> PushM Database t Database
insertTuples _ TblEmpty db _ = return db
insertTuples _ TblFull db  _ = return db
insertTuples tblPair exp@(Tbl ref vars heads) db _
    | nullTablePair tblPair = return db
    | otherwise             = do
  let totalColumns = length vars
  
  let inject tup = \i -> tup ! (fromJust $ lookup i vars)
      
  (id, vars, _) <- liftPushMProvs State.get

  let content' = map
                 (\(Tuple tup1 tup2) -> 
                      Tuple (Vect.map (inject tup2)
                             (Vect.fromList [0..(totalColumns - 1)]))
                      (TheoryBlame id $ Map.fromList $ zip vars 
                                      $ termsOf (Vect.toList tup1)))
                 (DB.contents tblPair)


  -- Filter the tuples that already exist in uni
  uni <- liftPushMBase $ State.get
  let tableInUni = DB.contents $ Map.findWithDefault emptyTable ref uni
  let content''  = filter (\t -> let t' = undecorate t 
                                 in  not $ t' `elem` tableInUni) content'

  let newProvs = Map.fromList 
                 $ (\(Tuple tup blm) -> (obsOf ref (Vect.toList tup), blm)) 
                 <$> content''

  liftPushMProvs $ State.modify
                 $ \(_, _, ps) -> 
                     let oldProvs = observationProvs ps                         
                     in  ( id
                         , vars
                         , ps { observationProvs = Map.union oldProvs newProvs })

  return $  Map.insertWith unionTables ref 
             (undecorateTable $ DB.Set content'') db
    -- inserting with DB.union here does not allow for duplicate entries.
    where obsOf (ConstTable c) [e] = Obs $ Rel "=" [Cons c, Elem e]
          obsOf (RelTable r) es    = Obs $ Rel r $ termsOf es
          obsOf (FnTable f ) es    = Obs $ Rel f $ termsOf es
          termsOf es   = Elem <$> es


insertTuples tblPair exp@(Proj innerExp col heading skFn unqExp) db depth
    | nullTablePair tblPair = return db
    | otherwise             = do
  let totalColumns          = length $ Map.keys $ header innerExp
                          -- we have this many columns in total
  uni <- liftPushMBase State.get
                          -- entire set of facts from the previous iteration of
                          -- the Chase
  provs <- liftPushMProvs $ State.get

  let diff = DB.contents tblPair
  -- MONITOR >
             -- tuples to insert (tuples that already do not exists)
  let inject = case unqExp of
                 Nothing -> insertProjInject col
                 Just ue -> insertProjUniqueInject db uni ue heading col
               -- Create a function for injecting the projected out columns.
               -- The function depends on the value of unique expression.

  (_, _, allProvs) <- (liftPushMProvs State.get)
  let provs = elementProvs allProvs
  new   <- foldM (\set (Tuple tup1 tup2) -> do
                          let ds = maximum <$>
                                   (termDepth <$>) <$>
                                   (flip getElementProv) provs <$> 
                                   tup1
                          if depth > -1 && Vect.any (>= depth) ds
                          then return set
                          else do
                            tup2' <- (Vect.mapM (inject (tuple tup2))
                                     (Vect.fromList [0..(totalColumns - 1)]))
                            return ((Tuple tup1 tup2'):set)) 
           empty diff

  liftPushMProvs $ State.modify 
                 $ \(id, vs, ps) -> (id, vs, newElementsProvs new skFn col ps)
                                    
  insertTuples (DB.Set new) innerExp db depth

insertTuples tbl@(DB.Set set) (Sel exp colPairs _) db depth = do
  let totalColumns = length colPairs
  let inject tup = Vect.map 
                   (\i -> case lookup i colPairs of
                            Nothing -> error $ 
                                       unitName ++ ".insertTuples: " ++
                                       error_invalidRelExp
                            Just j  -> tup ! j) 
                   $ Vect.fromList [0..(totalColumns - 1)]
  let new        = map (\(Tuple tup1 tup2) -> Tuple tup1 (inject tup2))
                   set
  insertTuples (DB.Set new) exp db depth
             
insertTuples tbl@(DB.Set set) exp@(Join lExp rExp heads) db depth = do
  let lTran = unjoinTupleTransformer (header lExp) heads
  let rTran = unjoinTupleTransformer (header rExp) heads
  let lSet  = map (\(Tuple tup1 tup2) -> Tuple tup1 (lTran tup2)) set
  let rSet  = map (\(Tuple tup1 tup2) -> Tuple tup1 (rTran tup2)) set

  db' <- insertTuples (DB.Set lSet) lExp db depth
  insertTuples (DB.Set rSet) rExp db' depth
insertTuples tbl (Delta _ _)          _  _ = 
    error $ unitName ++ ".insertTuples: " ++ error_insertIntoDeltaView
insertTuples tbl (Union _ _ _ _ _   ) _  _ =
    error $ unitName ++ ".insertTuples: " ++ error_insertIntoUnionView

{- Acts as a helper for 'insertTuple' when inserting into a Proj expression.
   This function returns a funciton that mapas elements of a target 'Tuple'
   according to a given source 'Tuple' and a projection column number.

   Inputs:
   - projecting column number of type 'Int'
   - a source tuple of type 'Tuple'

   Output: a function that maps an index number to an 'Element' in a PushM
   context
-}
insertProjInject :: Int -> Tuple -> (Int -> PushM Database t Element)
insertProjInject col (Tuple vs _) = 
    \i -> if   i == col
          then liftPushMCounter freshElement
          else let j = if i > col then i - 1 else i
               in return $ vs ! j
               -- a helper function to inject a fresh element at its
               -- position in a projected tuple (being inserted)

{- Like inserProjInject, acts a helper for 'insertTuple' when inserting into a 
   Proj expression. This function is employed when a funcitonal dependency must
   be enforced on a function table according to a unique expression and returns
   a funciton that maps elements of a target 'Tuple' according to a given 
   source 'Tuple' and a projection column number.

   Inputs:
   - the current database of type 'Database'
   - the current union database of type 'Database'
   - the relational expression enforcing unique values
   - header of the project expression
   - projecting column number of type 'Int'
   - a source tuple of type 'Tuple'

   Output: a function that maps an index number to an 'Element' in a PushM
   context
-}
insertProjUniqueInject :: Database -> Database -> RelExp -> Header
                       -> Int -> Tuple -> (Int -> PushM Database t Element)
insertProjUniqueInject db uni unqExp heading col tup@(Tuple vs _) =     
    let unqTblInNew = undecorateTable (evaluateRelExpNoDelta db unqExp)
        unqTblInUni = undecorateTable (evaluateRelExpNoDelta uni unqExp)
        fetch       = \ftbl -> 
                      fetchUnique tup heading (header unqExp) col ftbl
    in \i -> if   i == col
             then case (fetch unqTblInNew) <|> (fetch unqTblInUni) of
                    Nothing  -> liftPushMCounter freshElement
                    Just elm -> return elm
             else let j = if i > col then i - 1 else i
                  in return $ vs ! j


fetchUnique :: Tuple -> Header -> Header -> Column 
            -> Table -> Maybe Element
fetchUnique (Tuple vs _) tupHdr expHdr projCol tbl = 
    let hdr     = Map.intersection tupHdr expHdr        
        -- creating pairs of positions in the unique expression and the values
        -- that they are supposed to get (conisdering the tuple that is about
        -- to be inserted):
        colVals = Map.elems $ Map.mapWithKey (\a c -> 
                                          ( fromJust $ Map.lookup a expHdr
                                          , vs ! c ) ) hdr
   in let sel           = columnValuesSelector colVals
          (DB.Set set') = if null colVals 
                          -- < MONITOR
                          -- If we decide to be generous with creating new 
                          -- elements, this has to be emptyTable:
                          then tbl
                          -- MONITOR >
                          else DB.select sel tbl
      in  if   null set'
          then Nothing
          else Just $ Vect.last (tupleElems $ head set')

{- A helper function for computing the provenance information for a set of 
   elements when inserting the elements into a projected column. -}
newElementsProvs :: [TuplePair] -> FnSym -> Int -> ProvInfo -> ProvInfo
newElementsProvs setPair fSym col provs =
   let pairs = map (\(Tuple tup1 tup2) -> (tup2 ! col, tup1)) setPair
   in  foldr  (\(e, es) -> modifyElementProvs 
               $ addElementProv e fSym (Vect.toList es)) provs pairs


{-| Given a two 'Header' corresponding to 'RelExp's for body and head of a 
  sequent, returns a transforming function that maps the tuples in the body
  to tuples in the head. The function is used by 'diffRelExpSets' to compute the
  difference set resulting from evaluating the body and head in a database.
-}
tupleTransformer :: Header -> Header -> (Tup -> Tup)
tupleTransformer heads1 heads2 =
    let colPairs  = Map.fromList.Map.elems $
                    Map.mapWithKey (\k i -> 
                            case Map.lookup k heads1 of
                              Nothing -> error $ unitName 
                                         ++ ".tupleTransformer: "
                                         ++ error_freeVarInHead
                              Just j  -> (i, j))  
                    heads2
        totalCols = Map.size heads2
    in  \tup -> 
        Vect.map (\i -> let j = fromJust $ Map.lookup i colPairs
                        in tup ! j)
                (Vect.fromList [0..(totalCols - 1)])

{- Given three 'Headers' for two sets that are about to be joined and the 
   header of the target set, returns a function that transforms the tuples of 
   the two joined sets to a tuple of the target set. -}
joinTupleTransformer :: Header -> Header -> Header 
                     -> (TupleSub -> TupleSub -> TupleSub)
joinTupleTransformer lHeader rHeader jHeader =
    let list              = swap <$> (Map.toList jHeader)
        jHeaderR          = Map.fromList list
        total             = length list - 1
        vectorTransformer = \lVs rVs -> 
          (Vect.map (\i -> case Map.lookup i jHeaderR of
                             Nothing -> error "error1"
                             Just a  -> case Map.lookup a lHeader of
                                          Nothing -> case Map.lookup a rHeader of
                                                       Nothing -> error "error2"
                                                       Just k  -> rVs ! k
                                          Just j  -> lVs ! j)
           (Vect.fromList [0..total]))
    in  \(Tuple lVs lIs) (Tuple rVs rIs) -> 
        Tuple (vectorTransformer lVs rVs) (Map.union lIs rIs)

unjoinTupleTransformer :: Header -> Header -> (Tup -> Tup)
unjoinTupleTransformer heads jHeads =
    let list   = swap <$> (Map.toList heads)
        headsR = Map.fromList list
        total  = length list - 1
    in  \tup -> 
        (Vect.map (\i ->
                       case Map.lookup i headsR of
                         Nothing -> error "error1"
                         Just a  -> case Map.lookup a jHeads of
                                      Nothing -> error "error2"
                                      Just j  -> (tup ! j))
         (Vect.fromList [0..total]))