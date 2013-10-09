module Chase.Problem.IRelAlg where


-- General Modules
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities

-- Chase Modules:
import Chase.Problem.Observation
import qualified CC.CC as CC
import qualified RelAlg.DB as RA

-- Other Modules
import Debug.Trace


{- Data Types -}

{- A record in a table in models. -}
type Record = [Term]

{- A table in models. -}
type Table = RA.Set Record

{- A map from relational symbols to tables -}
type Tables = Map.Map Sym Table


-- THIS HAS TO BE MOVED SOMEWHERE ELSE!
instance Functor RA.Set where
    fmap f (RA.Set x) = RA.Set (map f x)

instance Applicative RA.Set where
    pure x                = RA.Set (pure x)
    RA.Set f <*> RA.Set x = RA.Set (f <*> x)

--------------------------------------------------------------------------------
-- Use frame instead of sequent. Then get rid of origin in Frame.
matchRA :: Sequent -> Tables -> [Sub]
matchRA (Sequent bdy hds) tbls = 
    --(trace.show) tbls
    createSubs (snd bdySet) facts
    where bdySet@(_, bdyLbls) = bodySet tbls bdy
          hdSets              = headSet tbls hds
          facts               = tableDiff bdySet hdSets
          noFVars lbls        = all (\l -> ((not.isJust) l) ||
                                           not(l `elem` bdyLbls)) lbls

tableDiff :: (RA.Set [Term], [Maybe Var]) -> [(RA.Set [Term], [Maybe Var])]
          -> RA.Set [Term]
tableDiff (bdySet, bdyLbls') hds' =
    let (facts, _) = unzip
                    (filter (\(_, sel) -> sel == False) 
                     (zip (RA.toList bdySet) slctList))
                      -- using filter instead of RA.select
    in RA.Set facts
    where bdyLbls    = remDupVars bdyLbls'
          hds        = map (\(s, l) -> (s, remDupVars l)) hds'
          hlsps      = (\(_, hdLbls) -> 
                            unzip [(x, y) | (x, y) <- zip hdLbls [0..]
                                  , isJust x
                                  , x `elem` bdyLbls]) <$> hds
                       -- Filter variable positions in the head
          blsps      = (\(_, hdLbls) -> 
                            unzip [(x,y) | (x, y) <- zip bdyLbls [0..]
                                  , isJust x
                                  , x `elem` hdLbls]) <$> hds
                       -- Filter variable positions in body, variables that
                       -- show up somewhere in the head.
          refInds    = (\(bls, _) -> zip bls [0..]) <$> blsps
                       -- Give new indices to variables in the body. We use
                       -- these indices to reorder the head set such that it
                       -- aligns with the body set.
          hdProj     = (\(_, hps) -> 
                        RA.Proj $ \(RA.Set ts) -> 
                            RA.Set (indProjList hps <$> ts)) <$> hlsps
          projHd     = (\((hdSet, _), p) -> RA.ext p hdSet) <$> zip hds hdProj
                       -- Project the head set: 
                       -- choose only the variable positions.
          bdyProj    = (\(_, bps) -> 
                            RA.Proj $ \(RA.Set ts) -> 
                                RA.Set (indProjList bps <$> ts)) <$> blsps
          projBdy    = (\p -> RA.ext p bdySet) <$> bdyProj
                       -- Project the body set:
                       -- choos only those variable positions that show up in
                       -- the head.
          lkUp       = (\((hls, _), ref)  -> [fromJust y | x <- hls
                                             , let y = lookup x ref
                                             , isJust y]) <$> zip hlsps refInds
                       -- Create a pattern for reordering the head set. We want
                       -- to align the head and the body set.
          reordHd    = (\(lu, p) -> RA.Set ((reordList lu) <$>  p))
                       <$> zip lkUp (RA.toList <$> projHd)
                       -- Of course, reorder the head set.
          slctList   = foldr1 (zipWith (||)) temp
          temp       = (\(b,h) -> tblSlct (RA.toList b) (RA.toList h)) 
                       <$> zip projBdy reordHd
          tblSlct t1 t2 = (\x -> x `elem` t2) <$> t1
          -- bdyPred    = RA.Pred $ \x -> 
          --              and ((\s -> not (x `elem` s)) 
          --                   <$> (RA.toList <$> reordHd))
                       --and (not (x `elem` (RA.toList reordHd)))
                       -- Choose those records in the body that do not exist
                       -- in the head.

          
{- Given the head of a geometric sequent and a set of tables, returns a list of
data views for the relational expressions corresponding to the disjuncts in the
head formula.
-}
headSet :: Tables -> Formula -> [(RA.Set [Term], [Maybe Var])]
headSet tbls (Or f1 f2)    = 
    headSet tbls f1 ++ headSet tbls f2 -- Disjunctions at the top level
headSet tbls (Exists x f1) =
    headSet tbls f1  -- NOT SURE!
headSet tbls fmla          = [formulaSet tbls fmla]

{- Given the body of a geometric sequent and a set of tables, returns a data 
view corresponding to the formula's relational expression.
-}
bodySet :: Tables -> Formula -> (RA.Set [Term], [Maybe Var])
bodySet =  formulaSet

{- Computes the output of formulaSet. This function maintains a list of terms 
appearing in the atomic formulas as it proceeds. This list is used to compute
joins between tables over common variables in atomic formulas.
-}
formulaSet :: Tables -> Formula -> (RA.Set [Term], [Maybe Var])
formulaSet _ Fls                 = (RA.Set [], [])
formulaSet _ Tru                 = (RA.Set [[Elm "True"]], [Nothing])
-- NOT SURE
formulaSet tbls (Atm a@(R _ ts)) = atomSet a tbls
formulaSet tbls (And f1 f2)          = 
    joinHelper (\(x,y) -> True) res1 res2 -- Join the two sets
    where res1 = formulaSet tbls f1  -- simple recursion
          res2 = formulaSet tbls f2

atomSet :: Atom -> Tables -> (RA.Set [Term], [Maybe Var])
atomSet a@(R "=" [t1, t2]) tbls =
    let res1 = termSet t1 tbls
        res2 = termSet t2 tbls
    in  joinHelper (\_ -> True) res1 res2
atomSet a@(R sym ts) tbls = 
    case tbl of
      Nothing  -> (RA.Set [], [])
                  -- The table does not exist, no data found!
      Just t   -> let set = RA.select pred t
                  in  foldl foldFunc (set, lbls) $ zip ts [0..]
                  -- Select the records that match the terms of the relation
    where tbl  = Map.lookup sym tbls 
                 -- Lookup the table associated with this relation
          pred = RA.Pred $ \x -> constPredicates ts x && varPredicates ts x
          lbls = (\t -> case t of 
                         Var v     -> Just v
                         otherwise -> Nothing) <$> ts
                 -- Make a conjunction of two sets of predicates: 
                 -- (1) predicates that choose records whose entries match the 
                 -- constants in the atomic formula within the positions they 
                 -- appear, and (2) predicates that force positions equal 
                 -- content in columns whose corresponding variables are the 
                 -- same in the atomic formula.
          foldFunc (set, ls) (term, i) =  -- only care about variable labels
              case term of
                Var v     -> (set, ls)
                -- Fn sym [] -> (set, ts, i + 1)
                Elm _     -> (set, ls) 
                -- dealt with in constPredicates
                otherwise -> 
                    let (set', ls') = termSet term tbls
                        initPred    = \(x,y) -> 
                                      x !! i == 
                                        y !! ((length ls') - 1)
                    in (joinHelper initPred (set, ls) (set', ls')) 

-- NOTE: processing variables, constants and functions can happen in one run.
termSet :: Term -> Tables -> (RA.Set [Term], [Maybe Var])
termSet t@(Fn sym terms) tbls = 
    case tTable of
      []        -> (RA.Set [], []) -- maybe?!?
      otherwise -> foldl foldFunc (tSet, lbls) $ zip terms [0..]
    where tSet@(RA.Set tTable)  = funcSet t tbls
          lbls = (\t -> case t of 
                         Var v     -> Just v
                         otherwise -> Nothing) <$> terms <|> [Nothing]
          foldFunc (set, ls) (term, i) = 
              case term of
                Var v     -> (set, ls)
                --Fn sym [] -> (set, ts, i + 1)
                Elm sym   -> (set, ls )
                otherwise -> 
                    let (set', ls') = termSet term tbls
                        initPred    = \(x,y) -> 
                                      y !! i == 
                                        x !! ((length ls') - 1)
                    in (joinHelper initPred (set', ls') (set, ls)) 
termSet t _ = (error.show) t


funcSet :: Term -> Tables -> RA.Set [Term]
funcSet (Var _)   _      = error "Cannot be a variable!"
-- funcSet (Fn _ []) _      = error "Cannot be a constant!"
funcSet (Fn sym ts) tbls = 
    case tbl of
      Nothing  -> RA.Set []
                  -- The table does not exist, no data found!
      Just t   -> RA.select pred t
                  -- Select the records that match the terms of the relation
    where tbl  = Map.lookup sym tbls 
                 -- Lookup the table associated with this relation
                 -- Probably function lookup is different than table lookup!
          pred = RA.Pred $ \x -> constPredicates ts x && varPredicates ts x
                 -- Make a conjunction of two sets of predicates: 
                 -- (1) predicates that choose records whose entries match the 
                 -- constants in the atomic formula within the positions they 
                 -- appear, and (2) predicates that force positions equal 
                 -- content in columns whose corresponding variables are the 
                 -- same in the atomic formula.

{- Constructs predicates to filter the records of a set based on the constants 
appearing in the terms of an atomic formula. -}
constPredicates :: [Term] -> ([Term] -> Bool)
constPredicates ts = fst $ foldl foldFunc (\x -> True, 0) ts
    where foldFunc (pred, i) t = 
              case t of
                Elm _     -> ((\x -> (x !! i) == t --constant
                                   && (pred x)) , i + 1)
                
                -- Fn _ []   -> ((\x -> (x !! i) == t --constant
                --                    && (pred x)) , i + 1)
                otherwise -> (pred, i + 1)  -- otherwise, ignore

{- Constructs predicates to filter the records of a set with regards to the
positions of an atomic formula's terms that share the same variables.
-}
varPredicates :: [Term] -> ([Term] -> Bool)
varPredicates ts = 
    let varMap = createVarMap ts
    in  Map.fold foldPred (\x -> True) varMap
    where foldPred ps pred = 
              if length ps > 1
              then \x -> varPred ps x && pred x
              else pred

varJoinPredicates :: (([Term], [Term]) -> Bool) -> [Maybe Var] -> [Maybe Var]
                      -> (([Term], [Term]) -> Bool)
varJoinPredicates initPred lts rts = 
    foldr foldPred initPred $ Map.toList lVarMap
    where lVarMap               = head <$> createVarMap' lts
          rVarMap               = head <$> createVarMap' rts
          -- Construct two maps for the variables of the two sets of terms and
          -- only choose one of the positions for each variable (head).
          foldPred (v, lp) pred = 
              case Map.lookup v rVarMap of
                Nothing -> pred
                Just rp -> \xy -> varJoinPred lp rp xy && pred xy
              -- If a variable from the first map appears in a the second map,
              -- construct a predicate to force equality for the two 
              -- corresponding positions.

joinHelper :: (([Term], [Term]) -> Bool) -> (RA.Set [Term], [Maybe Var])
           -> (RA.Set [Term], [Maybe Var]) -> (RA.Set [Term], [Maybe Var])
joinHelper initPred (tbl1, ts1) (tbl2, ts2) = 
    (mergeSetPairs $ RA.join pred tbl1 tbl2, ts1 ++ ts2)
    where pred = RA.Pred $ varJoinPredicates initPred ts1 ts2
                 -- Construct a predicate for joining the two sets.

-- Helpers
{- Creates a map from the variables in a relation to the positions in which 
they occur. -}
{- Merges a pair of tables (wrapped inside one RA.Set) into one table. This 
kind of pairs are usually constructed by the join operation in Henglein's 
library. -}
mergeSetPairs :: RA.Set ([a], [a]) -> RA.Set [a]
mergeSetPairs tbl = mergeFunc <$> tbl
    where mergeFunc = \(a, b) -> (a ++ b)

createVarMap :: [Term] -> Map.Map Var [Int]
createVarMap ts = fst $ foldl foldFunc (Map.empty, 0) ts
    where foldFunc (m, i) t = 
              case t of
                Var t'    -> (Map.insertWith (++) t' [i] m, i + 1)
                othewrise -> (m, i + 1)

createVarMap' :: [Maybe Var] -> Map.Map Var [Int]
createVarMap' ts = fst $ foldl foldFunc (Map.empty, 0) ts
    where foldFunc (m, i) t = 
              case t of
                Just t'    -> (Map.insertWith (++) t' [i] m, i + 1)
                Nothing    -> (m, i + 1)

{- Reorders the data in a table according to a given pattern. -}
reordList :: [Int] -> [a] -> [a]
reordList _  []     = []
reordList [] _      = []
reordList (p:ps) xs = xs !! p: reordList ps xs

indProjList :: [Int] -> [a] -> [a]
indProjList ps xs = [xs !! p | p <- ps]

{- The next two functions are helpers for varPredicates and varJoinPredicates
respectively. -} 
varPred :: [Int] -> ([Term] -> Bool)
varPred ps = 
    \x -> allTheSame $ ($x) <$> fns
    where fns = (\p x -> x !! p) <$> ps


varJoinPred :: Int -> Int -> (([Term], [Term]) -> Bool)
varJoinPred lp rp = \(x,y) -> x !! lp == y !! rp


{- Returns true if all the elements of a list are the same. Otherwise, returns
false. -}
allTheSame :: Eq a => [a] -> Bool
allTheSame (x:xs) = and $ map (== x) xs

createSubs :: [Maybe Var] -> RA.Set [Term] -> [Sub]
createSubs vars (RA.Set [])  = []
createSubs vars (RA.Set set) = Map.fromList.subList <$> set
    where subList ts = [(fromJust v, t) | (v, t) <- zip vars ts, isJust v]

remDupVars :: [Maybe Var] -> [Maybe Var]
remDupVars vs = foldr foldFunc [] vs
    where foldFunc v vs' = if   v `elem` vs'
                           then Nothing:vs'
                           else v:vs'

--------------------------------------------------------------------------------
-- TEST
mkTable :: [[String]] -> RA.Set [Term]
mkTable strs = RA.Set $ ((parseTerm <$>) <$>) strs

-- setP = RA.Set [[Elm "c0", Elm "c1"], 
--                [Elm "c2", Elm "c3"], 
--                [Elm "c3", Elm "c3"]]
-- setQ = RA.Set [[Elm "c1"], 
--                [Elm "c3"]]
-- setR = RA.Set [[Elm "c1", Elm "c2", Elm "c2"], 
--                [Elm "c0", Elm "c0", Elm "c2"]]

-- setF = RA.Set $ [[Elm "c0", Elm "c2"], [Elm "c1", Elm "c3"]]

-- setA = RA.Set [[Elm "c0"]]
-- setB = RA.Set [[Elm "c1"]]

-- tables = Map.fromList [("P", setP), ("Q", setQ), ("R", setR), 
--                        ("f", setF), ("a", setA), ("b", setB)]

setA = RA.Set [[Elm "c1"]]
setB = RA.Set [[Elm "c2"]]
setP = RA.Set [[Elm "c1", Elm "c2"]]
setF = RA.Set []
setG = RA.Set []
tables = Map.fromList [("a", setA), ("b", setB)
                      ,("P", setP)
                      ,("f", setF), ("g", setG)]
seql = parseSequent "P(x, y) => f(x) = g(f(y))"
out = matchRA seql tables