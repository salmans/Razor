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
import Data.List (elemIndex, find, maximumBy, nub, (\\), union)
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
    if   fmla1 == Tru || fmla2 == Tru
    then headRelExp Tru  -- shortcut Truth
    else fmla1' `union` fmla2'
    where fmla1' = headRelExp fmla1
          fmla2' = headRelExp fmla2
headRelExp fmla             = [formulaRelExp fmla']
    where fmla'  = removeEquations fmla

{- Creates a 'RelExp' for a 'Formula' in the body of a sequent. -}
bodyRelExp :: Formula -> RelExp
bodyRelExp = (formulaRelExp . removeEquations)

{- Replaces the equations in the a 'Formula' with Truth. This is necessary for 
   constructing the correct set of observational instances that are passed to 
   the SMT solver. -}
removeEquations :: Formula -> Formula
removeEquations fmla =
    let fmla' = removeEquationsHelper fmla
        diff  = (freeVars fmla) \\ (freeVars fmla')
    in  case (fmla', diff) of
          (f  , []) -> f
          (Tru, vs) -> let rels = (\v -> Atm $ Rel "@Element" [Var v]) <$> vs
                       in foldr1 And rels
          (f  , vs) -> let rels = (\v -> Atm $ Rel "@Element" [Var v]) <$> vs
                       in foldr And f rels

--  A helper for removeEquations:
removeEquationsHelper :: Formula -> Formula
removeEquationsHelper Tru = Tru
removeEquationsHelper Fls = Fls
removeEquationsHelper (Atm (Rel "=" _))  = Tru
removeEquationsHelper a@(Atm _)          = a
removeEquationsHelper (And fmla1 fmla2)  =
    case (removeEquationsHelper fmla1, removeEquationsHelper fmla2) of
      (Fls, _  ) -> Fls
      (_  , Fls) -> Fls
      (Tru, f  ) -> f
      (f  , Tru) -> f
      (f  , f' ) -> And f f'
removeEquationsHelper (Or fmla1 fmla2)    =
    case (removeEquationsHelper fmla1, removeEquationsHelper fmla2) of
      (Fls, f  ) -> f
      (f  , Fls) -> f
      (f  , f' ) -> Or f f'
removeEquationsHelper (Exists fn x fmla)  = 
    Exists fn x (removeEquationsHelper fmla)
removeEquationsHelper (Lone sk x fmla unq) = 
    Lone sk x (removeEquationsHelper fmla) unq

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
formulaRelExp fmla = error (show fmla)

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
        in  Join lExp rExp newHeader 
                (joinTupleTransformer lHeader rHeader newHeader)

{- As a helper for joinRelExp, computes the header of a join expression for two
   expressions with the two input headers -}
joinHeader :: Header -> Header -> Header
joinHeader lHeader rHeader =
    let rHeader'    = Map.difference rHeader lHeader
                    -- items that only exist in rHeader
        lHeaderSize = Map.size lHeader
        lHeader''   = Map.fromList $ zip (Map.keys lHeader) [0..]
        rHeader''   = Map.fromList $ zip (Map.keys rHeader') [lHeaderSize..]
                      -- shift the elements in the left and the right 
                      -- header, ignore the columns with no 
                      -- variable attributes
    in Map.unionWith const lHeader'' rHeader''

{- Given two 'Header's for two sets that are being joined and a header for the 
   target (joined) set, returns a function that transforms the tuples of 
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
delta TblEmpty                  = TblEmpty
delta TblFull                   = TblFull 
    -- This assumes sequents with TblFull on their left are processed once at the 
    -- beginning and will be never processed again.
delta exp@(Tbl _ _ hds)         = Delta exp hds
delta (Proj exp cols hds fn lf) = Proj (delta exp) cols hds fn lf
delta (Sel exp cols hds)        = Sel (delta exp) cols hds
delta (Join exp1 exp2 hds tran) = Union exp1 dlt1 exp2 dlt2 hds tran
    where dlt1 = delta exp1
          dlt2 = delta exp2
delta _                         = error $ unitName ++ ".delta: " ++ 
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
   - A 'TableSub' instance containging the resulting tuples and their 
   accompanying 'ExistsSub' substitutions to contain values for the columns that
   are projected out.
   NOTE: in an alternative implementation where values of Skolem functions are
   stored as tables in the database, values for the columns that are projected
   out are naturally returned as extra columns in the result set.
-}
{- Future extension: as a safety constraint, the header of the expression may be
   compared against the schema for the table being fetched from the database -}
evaluateRelExp :: Database -> Database -> RelExp -> TableSub
evaluateRelExp _ _ TblEmpty    = emptyTableSub
evaluateRelExp _ _ TblFull     = fullTableSub
evaluateRelExp db _ exp@(Tbl t _ _) = 
    let set = Map.findWithDefault emptyTable t db
    in  decorateTable set emptyExistsSub
        -- Decorate the resulting table with empty substituions (no columns to
        -- project out)
evaluateRelExp _ dlt (Delta exp@(Tbl t _ heads) _) =
    let set = Map.findWithDefault emptyTable t dlt
    in  decorateTable set emptyExistsSub
evaluateRelExp db delts (Proj exp col _ fn _) =
  let set  = evaluateRelExp db delts exp                                  
  in if   nullTableSub set                -- if the non-projected set is empty
     then evaluateRelExp db delts TblEmpty -- evaluate and return empty table
     else DB.project (deleteColumnProjector col updateFn) set
    where updateFn = Just $ \(Tuple v d) -> Map.insert fn (Elem (v ! col)) d
          -- function for updating the tuple's substitution
evaluateRelExp db delts (Sel exp pairs _) =
    let set    = evaluateRelExp db delts exp
        selFun = similarColumnsSelector pairs
    in  DB.select selFun set -- Select columns that are equal
evaluateRelExp db delts exp@(Join lExp rExp heads trans) =
    let lHds  = header lExp
        rHds  = header rExp
                -- Headers of the inner expressions are used to compute a 
                -- transforming function that constructs the join table from 
                -- the results of evaluating the two inner expressions.
        lSet  = evaluateRelExp db delts lExp
        rSet  = evaluateRelExp db delts rExp
                -- Well... evaluate the inner expressions first
        ps    = Map.elems $ Map.intersectionWith (,) lHds rHds
                -- Compute a set of pairs of indices: these pairs indicate 
                -- columns that must have equal values (we are jointing two 
                -- tables, right?)
        cond  = DB.Select $ \(Tuple x _ , Tuple y _) -> 
                and $ (\(p1, p2) -> x ! p1 == y ! p2) <$> ps
                -- Construct a filter function (of type DB.Select) to filter out
                -- records that have equal values in columns with the same 
                -- headers.
        tran  = \t1 t2 -> joinTupleTransformer lHds rHds heads t1 t2
    in  DB.map (uncurry tran) $ DB.join cond lSet rSet
        -- Construct the join table, apply the transforming function and return
        -- the result.
evaluateRelExp db delts exp@(Union lExp lDlt rExp rDlt heads tran) =
        -- Union expressions are like join expressions: we have to evaluate the
        -- inner expressions, apply a transforming function to construct tables
        -- that match the advertised header, and union the results.
    let lHds = header lExp
        rHds = header rExp
               -- Headers of lDlt and rDlt must be the same as headers of lExp
               -- and rExp unless something is wrong!
        ps   = Map.elems $ Map.intersectionWith (,) lHds rHds
        cond = DB.Select $ \(Tuple x _, Tuple y _) -> 
               and $ (\(p1, p2) -> x ! p1 == y ! p2) <$> ps

        set1 = DB.map (uncurry tran) $ DB.join cond lExpTbl rDltTbl
        set2 = DB.map (uncurry tran) $ DB.join cond lDltTbl rExpTbl
        set3 = DB.map (uncurry tran) $ DB.join cond lDltTbl rDltTbl
    in  unionsTables [set1, set2, set3]
    where lExpTbl = evaluateRelExp db delts lExp
          rExpTbl = evaluateRelExp db delts rExp
          lDltTbl = evaluateRelExp db delts lDlt
          rDltTbl = evaluateRelExp db delts rDlt
--------------------------------------------------------------------------------
{- Inserts the tuples of a 'TablePair' @t@ to a view for relational expression 
   @e@ in a database @db@. The schema for @t@ must be compatible with @e@, 
   otherwise, the function halts with error. 
   
   NOTE: the function assumes that the expression does not contain any delta
   subexpressions (unlike 'evaluateRelExp'). -}
insertTuples :: TablePair -> RelExp -> Database -> Int 
             -> PushM Database t Database
insertTuples _ TblEmpty db _ = return db
insertTuples _ TblFull db  _ = return db
insertTuples tblPair (Tbl ref vars heads) db _
    | nullTablePair tblPair  = return db
    | otherwise              = do      
  (id, vs, _) <- liftPushMProvs State.get -- get information required for 
                                            -- constructing blaming data
  let content'   =  let makeSub = Map.fromList . zip vs . toTerms . Vect.toList
                                 -- given a vector of tuples, create a 
                                 -- substitution from free variables of the 
                                 -- sequent to the elements in the tuple.
                    in  DB.map (\(Tuple t1 t2) -> 
                               (tuple t2, TheoryBlame id $ makeSub t1)) tblPair
                    -- Separate the tuples of the input 'TablePair', construct 
                    -- the second part to create a tuple that is inserted into
                    -- the target database, and the first tuple to create blaming
                    -- information.

  -- Filter out those tuples that already exist in the previous iterations of 
  -- the database (@uni@):
  uni <- liftPushMBase $ State.get
  let tableInUni = Map.findWithDefault emptyTable ref uni 
      -- target table in @uni@
  let content''  = DB.filter (\(t, _) -> not $ t `DB.elem` tableInUni) content'

  let newProvs   = Map.fromList 
                   $ (\(Tuple tup _, blm) -> (obsOf ref (Vect.toList tup), blm)) 
                   <$> (DB.toList content'') 
                   -- map observations to their corresponding blaming information

  liftPushMProvs $ State.modify
                 $ \(_, _, ps) -> 
                     let oldProvs = observationProvs ps                         
                     in  ( id
                         , vs
                         , ps { observationProvs = Map.union oldProvs newProvs })
                     -- Update the provenance information with the new blaming
                     -- data

  return $ Map.insertWith unionTables ref (nubTable $ DB.map fst content'') db 
           -- Removing duplicates in the new tuples for efficiency although
           -- inserting with DB.union here does not allow for duplicate entries.
    where obsOf (ConstTable c) [e] = Obs $ Rel "=" [Cons c, Elem e]
          obsOf (RelTable r) es    = Obs $ Rel r $ toTerms es
          obsOf (FnTable f ) es    = Obs $ Rel f $ toTerms es
          toTerms es               = Elem <$> es


insertTuples tblPair (Proj innerExp col heading skFn unqExp) db depth
    | nullTablePair tblPair = return db
    | otherwise             = do
  let totalColumns          = Map.size $ header innerExp
                          -- we have this many columns in total
  uni   <- liftPushMBase State.get
                          -- entire set of facts from the previous iteration of
                          -- the Chase

  let inject = case unqExp of
                 Nothing -> insertProjInject col
                 Just ue -> let inNew = fetchUniqueTable db ue
                                inUni = fetchUniqueTable uni ue 
                            in  insertProjUniqueInject inNew inUni (header ue) 
                                  heading col
               -- Create a function for injecting the projected out columns.
               -- If the unique expression is a Just value, the injecting 
               -- function looks up the new element that is about to be created
               -- in their unique functional tables and returns their old values
               -- if their exist.

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
           empty $ DB.toList tblPair
           -- Use the injecting function to insert the projected column.

  liftPushMProvs $ State.modify 
                 $ \(id, vs, ps) -> (id, vs, newElementsProvs new skFn col ps)
                                    
  insertTuples (DB.fromList new) innerExp db depth
  where fetchUniqueTable :: Database -> RelExp -> Table
        fetchUniqueTable db (Tbl t _ _) = Map.findWithDefault emptyTable t db

insertTuples tbl (Sel exp colPairs _) db depth = do
  let totalColumns = length colPairs
  let inject tup = Vect.map 
                   (\i -> case lookup i colPairs of
                            Nothing -> error $ 
                                       unitName ++ ".insertTuples: " ++
                                       error_invalidRelExp
                            Just j  -> tup ! j) 
                   $ Vect.fromList [0..(totalColumns - 1)]
  let new        = DB.map (\(Tuple tup1 tup2) -> Tuple tup1 (inject tup2)) tbl
  insertTuples new exp db depth
             
insertTuples tbl exp@(Join lExp rExp heads _) db depth = do
  let lTran = unjoinTupleTransformer (header lExp) heads
  let rTran = unjoinTupleTransformer (header rExp) heads
  let lSet  = DB.map (\(Tuple tup1 tup2) -> Tuple tup1 (lTran tup2)) tbl
  let rSet  = DB.map (\(Tuple tup1 tup2) -> Tuple tup1 (rTran tup2)) tbl

  db' <- insertTuples lSet lExp db depth
  insertTuples rSet rExp db' depth
insertTuples tbl (Delta _ _)          _  _ = 
    error $ unitName ++ ".insertTuples: " ++ error_insertIntoDeltaView
insertTuples tbl (Union _ _ _ _ _ _ ) _  _ =
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
insertProjUniqueInject :: Table -> Table -> Header -> Header
                       -> Int -> Tuple -> (Int -> PushM Database t Element)
insertProjUniqueInject unqInNew unqInUni unqHdr hdr col tup@(Tuple vs _) =     
    let fetch       = \ftbl -> fetchUnique tup hdr unqHdr col ftbl
    in \i -> if   i == col
             then case (fetch unqInNew) <|> (fetch unqInUni) of
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
   in let sel   = columnValuesSelector colVals
          set   = if null colVals 
                  -- < MONITOR
                  -- If we decide to be generous with creating new 
                  -- elements, this has to be emptyTable:
                  then tbl
                  -- MONITOR >
                  else DB.select sel tbl
      in  if   DB.null set
          then Nothing
          else Just $ Vect.last (tupleElems $ DB.oneMember set)

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