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

{- Relational expression, corresponding to a view for a geometric formula
   (without disjunction) -}
data RelExp = TblEmpty
            | TblFull
            | Tbl  Sym
            | Proj RelExp [Int]
            | Slct RelExp [(Int, Int)] [(Int, Term)]
            | Join RelExp RelExp [(Int, Int)]
            deriving (Show)

-- THIS HAS TO BE MOVED SOMEWHERE ELSE!
instance Functor RA.Set where
    fmap f (RA.Set x) = RA.Set (map f x)

instance Applicative RA.Set where
    pure x                = RA.Set (pure x)
    RA.Set f <*> RA.Set x = RA.Set (f <*> x)

{- A column label is either a variable or Nothing -}
type Label  = Maybe Var
type Labels = [Maybe Var]
--------------------------------------------------------------------------------
relExp :: Formula -> (RelExp, Labels)
relExp fmla =
    (projectVars exp lbls, newLbls)
    where (exp, lbls) = formulaRelExp fmla
          newLbls     = nub $ filter (/= Nothing) lbls

projectVars :: RelExp -> Labels -> RelExp
projectVars exp lbls = 
    Proj exp pos
    where lblMap = createLabelMap lbls
          pos    = [fromJust p | l <- nub lbls, 
                    isJust l, 
                    let p = Map.lookup (fromJust l) lblMap, 
                    isJust p]

{- Builds a relational expression for a goemetric formula (disjunction free). 
 It also returns a set of variables that appear in every formula. -}
formulaRelExp :: Formula -> (RelExp, Labels)
formulaRelExp Fls                 = (TblEmpty, [])
formulaRelExp Tru                 = (TblFull , [Nothing])
formulaRelExp (Atm a)             = atomRelExp a
formulaRelExp (And fmla1 fmla2)   =
    if (fmla1 == Fls || fmla2 == Fls) 
    then (TblEmpty, []) -- shortcut Falsehood
    else joinRelExp res1 res2 -- Join
    where res1 = formulaRelExp fmla1  -- apply recursion
          res2 = formulaRelExp fmla2
formulaRelExp (Exists x fmla)     = (exp, newLbls)
    where (exp, lbls) = formulaRelExp fmla
          newLbls     = (\l -> if l == Just x then Nothing else l) <$> lbls
          -- Do not keep existential variables in labels.
    -- if   null ps
    -- then (exp, lbls) -- alternatively (FullTbl, [])
    -- else (Proj exp [pos], [Just x])
    -- where (exp, lbls)  = formulaRelExp fmla
    --       ps           = filter (\(v, _) -> (isJust v) && (fromJust v) == x) 
    --                      $ zip lbls [0..]
    --       pos          = snd $ head ps

atomRelExp :: Atom -> (RelExp, Labels)
atomRelExp a@(R "=" [t1, t2]) =
    let res1 = termRelExp t1
        res2 = termRelExp t2
    in  equalityRelExp res1 res2
atomRelExp a@(R sym ts)       = 
    let exp = if vPairs == [] && ePairs == []
              then Tbl sym
              else Slct (Tbl sym) vPairs ePairs
    in  foldl foldFunc (exp, lbls) $ zip ts [0..]
                  -- Select the records that match the terms of the relation
    where ePairs = elmPreds ts 
                 -- (1) Predicates that choose records whose entries match the 
                 -- elements in the atomic formula within the positions they 
                 -- appear.
          vPairs = varPreds ts
                 -- (2) predicates that force positions equal content in 
                 -- columns whose corresponding variables are the same in the 
                 -- atomic formula.
          lbls   = termLabels ts
          foldFunc (exp, ls) (term, i)  -- only care about variable labels
                 = case term of
                     Var v     -> (exp, ls)
                     -- dealt with in varPreds
                     Elm _     -> (exp, ls)
                     -- dealt with in elmPreds
                     Fn c []   -> (Join exp (Tbl c) [(i, 0)], ls ++ [Nothing])
                     -- nothing left but constants
                     otherwise -> error $ "Chase.Problem.IRelAlg.atomRelExp: " ++
                                  "function symbols are not permitted!"

{- Creates a join expression for two input relational expressions and their
   corresponding variable labels. -}
joinRelExp :: (RelExp, Labels) -> (RelExp, Labels) -> (RelExp, Labels)
joinRelExp (exp1, lbls1) (exp2, lbls2) = 
    (Join exp1 exp2 pairs, lbls1 ++ lbls2)
    where pairs = varJoinPairs lbls1 lbls2
          -- Construct a set of pairs, representing a joining predicate.

{- Creates a join expression for two input relational expressions and their
   corresponding variable labels. -}
-- Salman: simplify considering functions are not permitted.
equalityRelExp :: (RelExp, Labels) -> (RelExp, Labels) -> (RelExp, Labels)
equalityRelExp (exp1, lbls1) (exp2, lbls2) = 
    (Join exp1 exp2 [pair], lbls1 ++ lbls2)
    where pair = (length lbls1 - 1, length lbls2 - 1)
          -- Construct a pair for joining the two sets.

termRelExp :: Term -> (RelExp, Labels)
termRelExp (Var v)   = (Tbl "*", [Just v])
termRelExp (Fn c []) = (Tbl c, [Nothing])
termRelExp _         = error $ "Chase.Problem.IRelAlg.termRelExp: function " ++
                       "symbols are not permitted!"

{- Constructs a set of pairs to filter the records of a set based on the 
elements appearing in the terms of an atomic formula. -}
elmPreds :: [Term] -> [(Int, Term)]
elmPreds ts = fst $ foldl foldFunc ([], 0) ts
    where foldFunc (pairs, i) t = 
              case t of
                Elm _     -> ((i, t): pairs, i + 1)                
                otherwise -> (pairs, i + 1)  -- otherwise, ignore

{- Constructs a set of pairs to filter the records of a set based on the
   positions of an atomic formula's terms that share the same variables.
-}
varPreds :: [Term] -> [(Int, Int)]
varPreds ts = 
    let varMap = createVarMap ts
    in  Map.fold foldPred [] varMap
    where foldPred ps pairs = 
              if length ps > 1
              then (pairsOf ps) ++  pairs
              else pairs
          pairsOf pos        = (\t -> (head pos, t)) <$> (tail pos)

evaluateRelExp :: Tables -> RelExp -> RA.Set [Term]
evaluateRelExp _ TblEmpty = RA.Set []
evaluateRelExp _ TblFull  = RA.Set [[Elm "True"]]
evaluateRelExp tbls (Tbl t) = case Map.lookup t tbls of
                                Nothing -> RA.Set []
                                Just t' -> t'
evaluateRelExp tbls (Proj exp inds) = 
    if    (not.null) (RA.toList set) && null (RA.toList projected)
    then  evaluateRelExp tbls TblFull
    else  projected
    where set       = evaluateRelExp tbls exp
          projected = RA.project (buildProjPred inds) set
evaluateRelExp tbls (Slct exp vPairs ePairs) =
    RA.select pred set
    where set   = evaluateRelExp tbls exp
          pred  = RA.Pred $ \x -> vPred x && ePred x
          vPred = \x -> and $ (\(p1, p2) -> x !! p1 == x !! p2) <$> vPairs
          ePred = \x -> and $ (\(p,  t ) -> x !! p  == t      ) <$> ePairs
evaluateRelExp tbls (Join exp1 exp2 pairs) = 
    mergeSetPairs $ RA.join pred set1 set2
    where set1  = evaluateRelExp tbls exp1
          set2  = evaluateRelExp tbls exp2
          pred  = RA.Pred $ \(x,y) -> 
                  and $ (\(p1, p2) -> x !! p1 == y !! p2) <$> pairs

-- Helper
{- Returns a list of labels (as needed by _RelExp functions) for a given set
 of terms. -}
termLabels :: [Term] -> Labels
termLabels ts = labelingFunc <$> ts
    where labelingFunc t = case t of 
                             Var v     -> Just v
                             otherwise -> Nothing

{- Creates a list of pairs, representing a join predicate for two set of input
 labels. -}
varJoinPairs :: Labels -> Labels -> [(Int, Int)]
varJoinPairs lLbls rLbls = 
    foldr foldPred [] $ Map.toList lVarMap
    where lVarMap               = createLabelMap lLbls
          rVarMap               = createLabelMap rLbls
          -- Construct two maps for the variables of the two sets of labels and
          -- only choose one of the positions for each variable (head).
          foldPred (v, lp) pairs = 
              case Map.lookup v rVarMap of
                Nothing -> pairs
                Just rp -> (lp, rp): pairs
              -- If a variable from the first map appears in a the second map,
              -- construct a pair to force equality for the two corresponding 
              -- positions.

{- Creates a map from a variable name to its positions in a list of variable
   labels.-}
createLabelMap :: Labels -> Map.Map Var Int
createLabelMap ts = fst $ foldl foldFunc (Map.empty, 0) ts                    
    where foldFunc (m, i) t = 
              case t of
                Just t'    -> (Map.insertWith const t' i m, i + 1)
                Nothing    -> (m, i + 1)

{- Just like createLabelMap, creates a map from a variable name to its 
   positions in a list of variable labels.-}
createVarMap :: [Term] -> Map.Map Var [Int]
createVarMap ts = fst $ foldl foldFunc (Map.empty, 0) ts
    where foldFunc (m, i) t = 
              case t of
                Var t'    -> (Map.insertWith (++) t' [i] m, i + 1)
                othewrise -> (m, i + 1)

{- Merges a pair of tables (wrapped inside one RA.Set) into one table. This 
kind of pairs are usually constructed by the join operation in Henglein's 
library. -}
mergeSetPairs :: RA.Set ([a], [a]) -> RA.Set [a]
mergeSetPairs tbl = mergeFunc <$> tbl
    where mergeFunc = \(a, b) -> (a ++ b)

{- Construct a projection predicate for a given set of indices. -}
buildProjPred :: [Int] -> RA.Proj [Term] [Term]
buildProjPred inds = RA.Proj (indProjList inds)

indProjList :: [Int] -> [a] -> [a]
indProjList ps xs = [xs !! p | p <- ps]

--------------------------------------------------------------------------------
-- Use frame instead of sequent. Then get rid of origin in Frame.
matchRA :: Sequent -> Tables -> [Sub]
matchRA seq@(Sequent bdy hds) tbls = 
    --(trace.show) seq
    --(trace.show) tbls
    --trace ("tableDiff  -> " ++ (show (tableDiff  bdySet hdSets)))
    --trace ("tableDiff' -> " ++ (show (tableDiff' bdySet hdSets)))
    --(trace.show) (createSubs (snd bdySet) facts)
    -- $
    createSubs (snd bdySet) facts
    where bdySet@(_, bdyLbls) = bodySet' tbls bdy
          hdSets              = headSet' tbls hds
          facts               = tableDiff' bdySet hdSets
          noFVars lbls        = all (\l -> ((not.isJust) l) ||
                                           not(l `elem` bdyLbls)) lbls


bodySet' tbls fmla = (evaluateRelExp tbls exp, lbls)
    where (exp, lbls) = relExp fmla


headSet' tbls (Or fmla1 fmla2) = headSet' tbls fmla1 ++ headSet' tbls fmla2
headSet' tbls fmla = [(evaluateRelExp tbls exp, lbls)]
    where (exp, lbls) = relExp fmla


tableDiff :: (RA.Set [Term], Labels) -> [(RA.Set [Term], Labels)]
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

{- New Implementation -}
tableDiff' :: (RA.Set [Term], Labels) -> [(RA.Set [Term], Labels)]
          -> RA.Set [Term]
tableDiff' bdy@(RA.Set set, _) hds = -- if hds is empty
    let temp = zip set conds
    in  RA.Set $ fst.unzip $ filter (\t -> snd t == True) temp
    where diffs = (tDiff bdy) <$> hds
          conds = foldr1 (zipWith (&&)) diffs

tDiff :: (RA.Set [Term], Labels) -> (RA.Set [Term], Labels) -> [Bool]
tDiff (bdySet, bdyLbls) (hdSet, hdLbls) = 
    --(trace.show) bdyLbls
    --(trace.show) hdLbls
    --(trace.show) bdySet
    --(trace.show) hdSet
    --(trace.show) ((\t -> not (t `elem` hdSet')) <$> bdySet')
    -- $
    (\t -> not (t `elem` hdSet')) <$> bdySet'
    where (bdyPrj, hdPrj) = sequentProjections bdyLbls hdLbls
          RA.Set bdySet'  = RA.project bdyPrj bdySet
          RA.Set hdSet'   = RA.project hdPrj hdSet

sequentProjections :: Labels -> Labels -> 
                    (RA.Proj [Term] [Term], RA.Proj [Term] [Term])
sequentProjections bdyLbls hdLbls = 
    (arrangeLabels bdyLbls' bdyRef, arrangeLabels hdLbls' hdRef)
    where bdyRef   = sortedReference bdyLbls
          hdRef    = sortedReference hdLbls
          bdyLbls' = filterLabels bdyLbls hdLbls
          hdLbls'  = filterLabels hdLbls bdyLbls'

{- Filters a list of labels according to a refrence set of labels. It removes
 the labels that do not show up in the reference set. -}
filterLabels :: Labels -> Labels -> Labels
filterLabels lbls refLbls = 
    filter ((flip elem) refLbls) varLbls
    where varLbls = filter (/= Nothing) lbls

{- Maps every variable in a list of tables to a position corresponding to the 
   variable in a sorted list of variables. -}
sortedReference :: Labels -> Map.Map Var Int
sortedReference lbls =     
    fst $ foldl foldFunc (Map.empty, 0) lbls
    where foldFunc (m, i) (Just v)
                     = (Map.insert v i m, i + 1)
          foldFunc (m, i) Nothing
                     = (m, i + 1)


{- Creates an RA.Proj instance that arranges a set of labels based on a set of
   reference positions maps. It simply ignores the labels that do not show up
   in the reference. -}
arrangeLabels :: Labels -> Map.Map Var Int -> RA.Proj [Term] [Term]
arrangeLabels lbls refMap = RA.Proj $ arrangeLabelsHelper lbls refMap

arrangeLabelsHelper :: Labels -> Map.Map Var Int -> [Term] -> [Term]
arrangeLabelsHelper lbls refMap ts =
    foldr arrangeFunc [] $ filter (/=Nothing) $ sort lbls
    where arrangeFunc (Just l) res = case Map.lookup l refMap of
                                       Nothing -> res
                                       Just p  -> ts !! p: res




{- Reorders the data in a table according to a given pattern. -}
reordList :: [Int] -> [a] -> [a]
reordList _  []     = []
reordList [] _      = []
reordList (p:ps) xs = xs !! p: reordList ps xs

createSubs :: Labels -> RA.Set [Term] -> [Sub]
createSubs vars (RA.Set [])  = []
createSubs vars (RA.Set set) = Map.fromList.subList <$> set
    where subList ts = [(fromJust v, t) | (v, t) <- zip vars ts, isJust v]

remDupVars :: Labels -> Labels
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
setP = RA.Set [[Elm "c1", Elm "c2"], [Elm "c2", Elm "c3"]]
setF = RA.Set [[Elm "c1", Elm "c2"]]
setG = RA.Set []
tables = Map.fromList [("a", setA), ("b", setB)
                      ,("P", setP)
                      ,("f", setF), ("g", setG)]
seql = parseSequent "P(x, y) => f(x) = g(f(y))"
out = matchRA seql tables