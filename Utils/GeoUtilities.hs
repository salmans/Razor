module Utils.GeoUtilities (isVar, 
--                          overatoms,
                          TermBased (..),
--                          onAtoms,
--                          atomUnion,
                          termVars,
                          functions,
--                          funcs,
                          predicates,
                          Sub,
                          Subst,
--                          variant,
--                          applyq,
--                          termval,
--                          theoryRelations,
--                          theoryFunctions,
--                          formulaRelations,
--                          formulaFunctions,
--                          termFunctions,
                          termFuncSyms,
--                          hasFuncSymbol,
                          closedTerm,
                          subterms,
--                          subtermPos,
--                          replaceSubterms,
                          Counter, freshVar, freshElement,
                          relConvert) where
{- Salman: Unexported functions may be removed! -}
-- 
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Tools.ListSet

import Formula.SyntaxGeo

import Control.Applicative
import Control.Monad.State as State

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
    fromTerm (Rn sym ts) = Just $ R sym ts
    fromTerm (Fn sym ts) = Just $ F sym ts
    fromTerm (Elm elm) = Just $ R elm []
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

    freeVars (Sequent body head) = List.union (freeVars body) (freeVars head)
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
atomUnion f fm = List.nub (overatoms (\h t -> f(h) ++ t) fm [])

-- LEFTOVER: (a list of all the variables occurring in a term)
termVars :: Term -> [Var]
termVars (Var v) = [v]
termVars (Elm _) = []
termVars (Fn fnName ts) = List.nub $ concatMap termVars ts

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
-- 
-- Substitute under a binder
-- The following functions need the type variable, as they are used at multiple types
-- 
variant :: Var -> Vars -> Var
variant x vars = if List.elem x vars then variant (x ++ "'") vars else x
-- 
applyq :: Sub -> (Var -> Formula -> Formula) 
       -> Var -> Formula -> Formula
applyq env quant x p = quant x' (apply ((Map.insert x (Var x')) env) p)
    where x' = if List.any (\k -> case Map.lookup k env of
                                  Just v -> List.elem x (freeVars v)
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
theoryRelations :: [Formula] -> [(Sym, Int)]
theoryRelations theory = List.nub $ concatMap formulaRelations theory

-- Returns a list of the functions in a theory
theoryFunctions :: [Formula] -> [(Sym, Int)]
theoryFunctions theory = List.nub $ concatMap formulaFunctions theory

-- Returns the relations in a formula
formulaRelations :: Formula -> [(Sym, Int)]
formulaRelations Tru = []
formulaRelations Fls = []
formulaRelations (Atm (R sym terms)) = (sym, length terms):[]
formulaRelations (And p q) = List.union (formulaRelations p) (formulaRelations q)
formulaRelations (Or p q) = List.union (formulaRelations p) (formulaRelations q)
formulaRelations (Exists x p) = formulaRelations p

-- Returns the functions in a formula
formulaFunctions :: Formula -> [(Sym, Int)]
formulaFunctions Tru = []
formulaFunctions Fls = []
formulaFunctions (Atm (R _ terms)) = List.nub $ concatMap termFunctions terms
formulaFunctions (And p q) = List.union (formulaFunctions p) (formulaFunctions q)
formulaFunctions (Or p q) = List.union (formulaFunctions p) (formulaFunctions q)
formulaFunctions (Exists x p) = formulaFunctions p

-- Returns the functions in a term
termFunctions :: Term -> [(Sym, Int)]
termFunctions (Var _) = []
termFunctions (Fn sym terms)
              | null terms = []
              | otherwise = 
                  List.nub $ (sym, length terms):(concatMap termFunctions terms)

-- Returns a list of function symbols in a term
termFuncSyms :: Term -> [Sym]
termFuncSyms (Var _) = []
termFuncSyms (Fn sym terms)
              | null terms = [sym]
              | otherwise = List.nub $ sym:(concatMap termFuncSyms terms)
termFuncSyms (Elm s) = [s] -- there are problems here


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
relConvert thy  = orig ++ integ
    where orig  = fst $ State.runState (theoryRelConvert thy) 0
          integ = integritySequents $ theoryFuncs thy
          

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

-- Create additional sequents to enforce function integrity constraints.
sequentFuncs :: Sequent -> [(Sym, Int)]
sequentFuncs (Sequent bdy hd) = (functions bdy) `List.union` (functions hd)

theoryFuncs :: Theory -> [(Sym, Int)]
theoryFuncs thy = filter (\(_, a) -> a /= 0) $ List.nub $ concatMap sequentFuncs thy

integritySequents :: [(Sym, Int)] -> [Sequent]
integritySequents fs = seq <$> fs
    where seq fa     = Sequent (lft fa) (parseFormula "y = y'")
          lft (f, a) = And (Atm (F f (vars a ++ [Var "y" ])))
                           (Atm (F f (vars a ++ [Var "y'"])))
          vars a     = (\i -> Var ("x" ++ show i)) <$> [1.. a]
---------------------------------------------
-- Convert for Relational Algebra
type Counter = State.State Int

freshVar :: Counter Var
freshVar = get >>= 
           (\c -> put (c + 1) >> 
           (return $ "x#" ++ (show c)))

freshElement :: Counter Term
freshElement = get >>=
               (\c -> put (c + 1) >>
               (return $ Elm $ "e#" ++ (show c)))