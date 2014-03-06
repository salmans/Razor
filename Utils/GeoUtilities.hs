module Utils.GeoUtilities ( isVar, TermBased (..), simplify, termVars
                          , functions, predicates, Sub, Subst
                          , theoryRelations, theoryFunctions, termFuncSyms
                          , closedTerm, subterms, replaceSubterms
                          , freshVar, freshElement, freshSymbol
                          , relConvert, addAllExistsPreds, addElementPred) where
{- Salman: Unexported functions may be removed! -}
-- 
import Data.List
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Tools.ListSet

import Formula.SyntaxGeo
import Tools.Counter

import Control.Applicative
import Control.Monad.State as State

import Debug.Trace

isVar :: Term -> Bool
isVar (Var _) = True
isVar _ = False
-- 
overatoms :: (Atom -> b -> b) -> Formula -> b -> b
overatoms f fm b = 
    case fm of 
      Atm a -> f a b
      Or p q -> over2 p q
      And p q -> over2 p q
      Exists _x p -> over1 p
      _ -> b
      where over1 p = overatoms f p b
            over2 p q = overatoms f p (overatoms f q b)
--
class TermBased a where
    liftTerm :: (Term -> Term) -> a -> a

    freeVars :: a -> Vars

    toTerm :: a -> Maybe Term
    toTerm _ = Nothing -- default implementation

    fromTerm :: Term -> Maybe a
    fromTerm _ = Nothing -- default implementation
--
instance TermBased Term where
    liftTerm f = f

    freeVars (Var x)     = [x]
    freeVars (Fn _ args) = Tools.ListSet.unions (map freeVars args)
    freeVars (Elm _)     = []

    toTerm = Just
    fromTerm = Just
--
instance TermBased Atom where
    liftTerm f (R sym terms) = R sym (map f terms)
    liftTerm f (F sym terms) = F sym (map f terms)

    freeVars (R _ args) = Tools.ListSet.unions (map freeVars args)
    freeVars (F _ args) = Tools.ListSet.unions (map freeVars args)
    
    toTerm (R sym terms) = Just $ Rn sym terms
    toTerm (F sym terms) = Just $ Fn sym terms

    fromTerm (Var _) = Nothing
    fromTerm (Rn sym ts)      = Just $ R sym ts
    fromTerm (Fn sym ts)      = Just $ F sym ts
    fromTerm (Elm (Elem elm)) = Just $ R elm []
--
instance TermBased Formula where
    liftTerm f = onAtoms (\atm -> case atm of
                                    (R p a) -> Atm(R p (map f a))
                                    (F p a) -> Atm(F p (map f a)))

    freeVars fm = case fm of
                    Tru -> []
                    Fls -> []
                    Exists x p -> freeVars p Tools.ListSet.\\ [x]
                    And p q -> combine p q
                    Or p q -> combine p q
                    Atm a -> freeVars a
        where combine p q = Tools.ListSet.union (freeVars p) (freeVars q)
--
instance TermBased Sequent where
    liftTerm f (Sequent b h) =
        Sequent (liftTerm f b) (liftTerm f h)

    freeVars (Sequent body head) = union (freeVars body) (freeVars head)
-- 
instance TermBased a => TermBased [a] where
    liftTerm f = map (liftTerm f)

    freeVars xs = Tools.ListSet.unions (map freeVars xs)
-- 
onAtoms :: (Atom -> Formula) -> Formula -> Formula
onAtoms f fm =
    case fm of 
      Tru -> Tru
      Fls -> Fls
      Atm a -> f a
      Or p q -> Or (onAtoms f p) (onAtoms f q)
      And p q -> And (onAtoms f p) (onAtoms f q)
      Exists x p -> Exists x (onAtoms f p)
--
atomUnion :: Eq b => (Atom -> [b]) -> Formula -> [b]
atomUnion f fm = nub (overatoms (\h t -> f(h) ++ t) fm [])

-- LEFTOVER: (a list of all the variables occurring in a term)
termVars :: Term -> [Var]
termVars (Var v) = [v]
termVars (Elm _) = []
termVars (Fn fnName ts) = nub $ concatMap termVars ts

-- %%% Function symbols with arity
-- 
functions :: Formula -> [(Sym, Int)]
functions = atomUnion (\(R _ args) -> foldr (Tools.ListSet.union . funcs) [] args) 
-- 
funcs :: Term -> [(Sym, Int)]
funcs (Var _) = []
funcs (Fn f args) = foldr (Tools.ListSet.union . funcs) [(f, length args)] args 
-- 
predicates :: Formula -> [(Sym, Int)]
predicates = atomUnion (\(R p args) -> [(p, length args)])
-- 
--
-- %%% Substitutions
-- 
type Sub = Map Var Term
-- 
--
-- %%% Substitutions
-- 
class Subst a where
  apply :: Sub -> a -> a
-- 
instance Subst Term where
  apply env tm = case tm of
                 Var x -> case Map.lookup x env of
                          Just t -> t
                          Nothing -> tm
                 Fn f args -> Fn f (map (apply env) args)
-- 
instance Subst Atom where
  apply env (R p args) = R p (map (apply env) args)
-- 
instance Subst Formula where
  apply env (Atm a) = Atm(apply env a)
  apply env (And p q) = And (apply env p) (apply env q)
  apply env (Or p q) = Or (apply env p) (apply env q)
  apply env (Exists x p) = applyq env Exists x p
  apply _ Tru = Tru
  apply _ Fls = Fls


-- Simplifying a formula
simplify :: Formula -> Formula
simplify (And p q)    = simplify1 (And (simplify p) (simplify q))
simplify (Or p q)     = simplify1 (Or (simplify p) (simplify q))
simplify (Exists x p) = simplify1 $ Exists x (simplify p)
simplify fm = fm

simplify1 :: Formula -> Formula
simplify1 fm = 
       case fm of
              Exists x p -> if elem x (freeVars p) then fm else p
              And Fls _  -> Fls
              And _ Fls -> Fls
              And Tru q  -> q
              And p Tru  -> p
              Or Fls q   -> q
              Or p Fls  -> p
              Or Tru _   -> Tru
              Or _ Tru   -> Tru
              _ -> fm              

-- 
-- Substitute under a binder
-- The following functions need the type variable, as they are used at multiple types
-- 
variant :: Var -> Vars -> Var
variant x vars = if elem x vars then variant (x ++ "'") vars else x
-- 
applyq :: Sub -> (Var -> Formula -> Formula) 
       -> Var -> Formula -> Formula
applyq env quant x p = quant x' (apply ((Map.insert x (Var x')) env) p)
    where x' = if any (\k -> case Map.lookup k env of
                                  Just v -> elem x (freeVars v)
                                  Nothing -> False) 
                           (freeVars p Tools.ListSet.\\ [x]) 
               then variant x (freeVars(apply (Map.delete x env) p)) else x
-- 
termval :: ([a], Var -> [b] -> b, Var -> [b] -> Bool) -> Map Var b -> Term -> b
termval m@(_, func, _) v tm =
  case tm of 
    Var x -> case Map.lookup x v of
               Nothing -> error "not in domain" 
               Just y -> y
    Fn f args -> func f (map (termval m v) args)
--
-- Returns a list of the relations in a theory
theoryRelations :: [Sequent] -> [(Sym, Int)]
theoryRelations theory = nub $ concatMap sequentRelations theory

sequentRelations :: Sequent -> [(Sym, Int)]
sequentRelations (Sequent bdy hd) = 
    nub $ formulaRelations bdy ++ formulaRelations hd

-- Returns a list of the functions in a theory
theoryFunctions :: [Sequent] -> [(Sym, Int)]
theoryFunctions theory = nub $ concatMap sequentFunctions theory

sequentFunctions :: Sequent -> [(Sym, Int)]
sequentFunctions (Sequent bdy hd) = 
    nub $ formulaFunctions bdy ++ formulaFunctions hd

-- Returns the relations in a formula
formulaRelations :: Formula -> [(Sym, Int)]
formulaRelations Tru = []
formulaRelations Fls = []
formulaRelations (Atm (R sym terms)) = (sym, length terms):[]
formulaRelations (And p q) = union (formulaRelations p) (formulaRelations q)
formulaRelations (Or p q) = union (formulaRelations p) (formulaRelations q)
formulaRelations (Exists x p) = formulaRelations p

-- Returns the functions in a formula
formulaFunctions :: Formula -> [(Sym, Int)]
formulaFunctions Tru = []
formulaFunctions Fls = []
formulaFunctions (Atm (R _ terms)) = nub $ concatMap termFunctions terms
formulaFunctions (And p q) = union (formulaFunctions p) (formulaFunctions q)
formulaFunctions (Or p q) = union (formulaFunctions p) (formulaFunctions q)
formulaFunctions (Exists x p) = formulaFunctions p

-- Returns the functions in a term
termFunctions :: Term -> [(Sym, Int)]
termFunctions (Var _) = []
termFunctions (Fn sym terms)
              | null terms = []
              | otherwise = 
                  nub $ (sym, length terms):(concatMap termFunctions terms)

-- Returns a list of function symbols in a term
termFuncSyms :: Term -> [Sym]
termFuncSyms (Var _) = []
termFuncSyms (Fn sym terms)
              | null terms = [sym]
              | otherwise = nub $ sym:(concatMap termFuncSyms terms)
termFuncSyms (Elm (Elem s)) = [s] -- there are problems here


-- Returns true if the input term contains a function with the given symbol.
hasFuncSymbol :: Sym -> Term -> Bool
hasFuncSymbol _ (Var x) = False
hasFuncSymbol sym (Fn f []) =
    if f == sym then True else False
hasFuncSymbol sym (Fn f terms) = 
    if f == sym then True else any (hasFuncSymbol sym) terms

-- Returns true if the given term has no variables; otherwise, returns false.
closedTerm :: Term -> Bool
closedTerm term = null $ termVars term

-- Returns all of the subterms of the input
subterms :: Term -> [Term]
subterms v@(Var _) = [v]
subterms t@(Elm _) = [t]
subterms t@(Fn f ts) = t:(concatMap subterms ts)

-- Returns all of the subterms of the input and their positions in the input
-- term (as their parent term)
subtermPos :: Term -> [(Term, Maybe Term)]
subtermPos v@(Var _) = [(v, Nothing)]
subtermPos t@(Elm _) = [(t, Nothing)]


-- Replaces all the occurances of the first in the last term with the second term.
replaceSubterms :: Term -> Term -> Term -> Term
replaceSubterms t1 t2 t3@(Var _) =
    if t3 == t1 
    then t2
    else t3
replaceSubterms t1 t2 t3@(Elm _) =
    if t3 == t1 
    then t2
    else t3
replaceSubterms t1 t2 t3@(Fn _ []) =
    if t3 == t1 
    then t2
    else t3
replaceSubterms t1 t2 t3@(Fn f terms) =
    if t3 == t1
    then t2
    else Fn f $ map (replaceSubterms t1 t2) terms

{- Eliminates all function symbols in the sequents of a theory and replaces 
  them with relational symbols. -}
relConvert :: Theory -> Theory
relConvert thy  = orig -- ++ total -- ++ integ
                  -- Salman: Because we have congruence closure, we don't need
                  -- the integrity sequents.
    where orig  = fst $ State.runState (theoryRelConvert thy) 0
          funcs = theoryFuncs thy
          integ = integritySequents funcs
          total = totalitySequents funcs
                   
{- Eliminates all function symbols in the sequents of a theory and replaces 
  them with relational symbols. -}
theoryRelConvert :: Theory -> Counter Theory
theoryRelConvert thy = mapM sequentRelConvert thy

{- Eliminates all function symbols in a sequent and replaces them with 
  relational symbols. -}
sequentRelConvert :: Sequent -> Counter Sequent
sequentRelConvert (Sequent bdy hd) = 
    formulaRelConvert bdy >>= 
    (\bdyres -> (formulaRelConvert hd >>=
    (\hdres  -> return $ Sequent (makeExists bdyres) (makeExists hdres))))
    where makeExists (fmla, vs) = foldr (\v f -> Exists v f) fmla vs

{- Eliminates all function symbols in a formula and replaces them with 
  relational symbols. It also retruns a list of existential variables introduced
  in this way. -}
formulaRelConvert :: Formula -> Counter (Formula, [Var])
formulaRelConvert Tru                = return (Tru, [])
formulaRelConvert Fls                = return (Fls, [])
formulaRelConvert (Atm (R sym ts))   = do
  (fs', ts', vs') <- foldM foldFunc ([], [], []) ts 
  return $ (andFmlas fs' (Atm (R sym ts')), vs')
    -- Replace ts with new variables computed by flattening terms. Also, 
    -- return a formula, corresponding to the conjunction of the formulas for
    -- flattening subterms. (This is pretty much like flattening terms below.)
    where andFmlas  = \fmlas fmla -> foldr (\f1 f2 -> And f1 f2) fmla fmlas
          foldFunc  = \(ffs, fts, fvs) t -> 
              do
                res <- termRelConvert t
                case res of
                  Nothing            -> 
                      return (ffs, fts ++ [t], fvs)
                  Just (tf, tt, tvs) -> 
                      return (ffs ++ [tf], fts ++ [tt], fvs ++ tvs)
formulaRelConvert (And fmla1 fmla2)  = do
  (fmla1', vs1) <- formulaRelConvert fmla1
  (fmla2', vs2) <- formulaRelConvert fmla2
  return $ (And fmla1' fmla2', vs1 ++ vs2)
formulaRelConvert (Or fmla1 fmla2)   = do
  res1 <- formulaRelConvert fmla1
  res2 <- formulaRelConvert fmla2
  return $ (Or (makeExists res1) (makeExists res2), [])
      -- Since disjunctions occur at the topmost level, apply existential
      -- quantification on the newly introduced variables before returning from
      -- this call.
  where makeExists (fmla, vs) = foldr (\v f -> Exists v f) fmla vs
formulaRelConvert (Exists v fmla)    = do
  (fmla', vs) <- formulaRelConvert fmla
  return $ (Exists v fmla', vs)

{- Eliminates all function symbols in a term and replaces them with relational 
   symbols. It also returns a set of existential variables introduced by the 
   process. -}
termRelConvert :: Term -> Counter (Maybe (Formula, Term, [Var]))
termRelConvert (Var v)   = return Nothing
termRelConvert (Fn _ []) = return Nothing
termRelConvert (Fn f ts) = do
  (fs', ts', vs') <- foldM foldFunc ([], [], []) ts
  v               <- freshVar
  let var         =  Var v
  return $ Just (andFmlas fs' (Atm (F f (ts' ++ [var]))), var, v:vs')
    where andFmlas  = \fmlas fmla -> foldr (\f1 f2 -> And f1 f2) fmla fmlas
          foldFunc  = \(ffs, fts, fvs) t -> -- for each subterm
              do
                res <- termRelConvert t -- flatten the subterm
                case res of             
                  Nothing            -> 
                      return (ffs, fts ++ [t], fvs)
                         -- If the subterm is either a constant or a variable,
                         -- use the subterm itself; no formula to conjoin.
                  Just (tf, tt, tvs) -> 
                      return (ffs ++ [tf], fts ++ [tt], fvs ++ tvs)
                         -- Otherwise, replace the term with a new variable
                         -- (returned by flattening the subterm); conjoin the
                         -- corresponding formula.

{- Applies addExistsPred on a theory of sequents. -}
addAllExistsPreds :: [Sequent] -> [Sequent]
addAllExistsPreds seqs = 
    State.evalState (run seqs) 0
    where run = foldM foldFunc []
          foldFunc res (Sequent b h) = do
            (h', seqs) <- addExistsPred h
            return $ res ++ (Sequent b h':seqs)


{- Since the current implementation of the chase requires every free variable
   on RHS of a sequent be mentioned on its LHS, we add a @Element predicate
   to the left of this kind of sequents. We also add @Element to the right of
   every sequent with existential quantifiers. -}
addElementPred :: Sequent -> Sequent
addElementPred seq = 
    let bdy' = foldr (\v b -> And (Atm (elementPred (Var v))) b) bdy hdFVars
        hd'  = addElementPredToRight [] hd
    in  seq { sequentBody = bdy' 
            , sequentHead = hd' }
    where hd      = sequentHead seq
          bdy     = sequentBody seq
          hdVars  = freeVars hd
          bdyVars = freeVars bdy
          hdFVars = hdVars \\ bdyVars
          allVars = hdVars `union` bdyVars

{- Uses addExistsPredHelper to relpace existential formulas, corresponding to 
   the head of a sequent, with fresh atomic formulas. As a consequence, new 
   sequents will be induced with the fresh atomic formula on left and 
   existential formula on right. The function returns the replaced head, from 
   the initial sequent, and the set of induced sequents. 
   The function requires a Counter to generate fresh relation symbols for the
   fresh atomic formulas. -}
addExistsPred :: Formula -> Counter (Formula, [Sequent])
addExistsPred fmla@(Or _ _) = addExistsPredHelper fmla
-- addExistsPred fmla@(Exists _ _) = addExistsPredHelper fmla
    -- If the head is an existential formula, it is already in the form we want.
    -- However, we want to generate a new predicate which can be used to 
    -- construct skolem terms for elements created by the existential 
    -- quantifier.
addExistsPred fmla          = return (fmla, [])

addExistsPredHelper :: Formula -> Counter (Formula, [Sequent])
addExistsPredHelper (Or f1 f2) = do
  (f1', seqs1) <- addExistsPredHelper f1
  (f2', seqs2) <- addExistsPredHelper f2
  return (Or f1' f2', seqs1 ++ seqs2)
addExistsPredHelper fmla@(Exists x f) = do
  let vs      = freeVars fmla
  sym         <- freshSymbol "@Exists"
  let atm     = R sym (map Var vs)
  let fmla'   = Atm atm
  (f', fSeqs) <- addExistsPredHelper f
  let seq     = Sequent fmla' (Exists x f')
  return (fmla', seq:fSeqs)
addExistsPredHelper f = return (f, []) -- Assumes normalized sequents

{- A helper for addElementPred, which adds @Element predicate to the right of 
   a sequent. The initial parameter of a list of variables is used to stack up
   all existentially quantified variables and add them in the most-inner level
   of the formula when the inductive steps are over. -}
addElementPredToRight :: Vars -> Formula -> Formula
addElementPredToRight _ (Or fmla1 fmla2) = -- go inside disjunction
    Or (addElementPredToRight [] fmla1) (addElementPredToRight [] fmla2)
    -- assuming normalized sequents
addElementPredToRight vs (And fmla1 fmla2) = 
    And (addElementPredToRight [] fmla1) (addElementPredToRight vs fmla2)
addElementPredToRight vs (Exists x fmla)  = -- add predicate for existentials
    Exists x (addElementPredToRight (x:vs) fmla)
addElementPredToRight vs fmla@(Atm atm@(R sym ts)) =
    let preds = (elementPred <$> ts') ++ ((elementPred.Var) <$> vs)
    in  foldr (\p f -> And (Atm p) f) fmla preds
                            -- This assumes flattened terms
    where ts' = filter (not.isVar) ts
addElementPredToRight _ fmla = fmla -- Note that we assume normalized sequents
                   

{- A helper for addElementPred. It constructs an @Element predicate for the 
   input term. -}
elementPred :: Term -> Atom
elementPred =  \x -> R "@Element" [x]

-- Create additional sequents to enforce function integrity constraints.
sequentFuncs :: Sequent -> [(Sym, Int)]
sequentFuncs (Sequent bdy hd) = (functions bdy) `union` (functions hd)

theoryFuncs :: Theory -> [(Sym, Int)]
theoryFuncs thy = filter (\(_, a) -> a /= 0) $ nub $ concatMap sequentFuncs thy

integritySequents :: [(Sym, Int)] -> [Sequent]
integritySequents fs = seq <$> fs
    where seq fa     = Sequent (lft fa) (parseFormula "y = y'")
          lft (f, a) = And (Atm (F f (vars a ++ [Var "y" ])))
                           (Atm (F f (vars a ++ [Var "y'"])))
          vars a     = (\i -> Var ("x" ++ show i)) <$> [1.. a]

totalitySequents :: [(Sym, Int)] -> [Sequent]
totalitySequents fs = seq <$> fs
    where seq (f, a) = Sequent Tru 
                       $ Exists "y" (Atm (F f (vars a ++ [Var "y"])))
          vars a     = (\i -> Var ("x" ++ (show i))) <$> [1..a]
---------------------------------------------