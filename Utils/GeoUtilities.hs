module Utils.GeoUtilities where
-- 
import Prelude 
import qualified List 
import qualified Char 
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Tools.ListSet
import Tools.ListSet((\\))

import Formula.SyntaxGeo

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
onSequent :: (Term -> Term) -> Sequent -> Sequent
onSequent f (Sequent b h) = 
    Sequent (onFormula f b) (onFormula f h)
-- 
onFormula :: (Term -> Term) -> Formula -> Formula
onFormula f = onAtoms(\(R p a) -> Atm(R p (map f a)))
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

onAtom :: (Term -> Term) -> Atom -> Atom
onAtom f (R sym terms) = R sym (map f terms)
--
atomUnion :: Eq b => (Atom -> [b]) -> Formula -> [b]
atomUnion f fm = List.nub (overatoms (\h t -> f(h) ++ t) fm [])
-- 

-- %%% Free Variables
-- 
class Fv a where
  fv :: a -> Vars
-- 
instance Fv Term where
  fv (Var x) = [x]
  fv (Fn _ args) = Tools.ListSet.unions (map fv args)
  fv (Elm _) = []
-- 
instance Fv Atom where
  fv (R _ args) = Tools.ListSet.unions (map fv args)
-- 
instance Fv Formula where
  fv fm = case fm of
          Tru -> []
          Fls -> []
          Exists x p -> fv p \\ [x]
          And p q -> combine p q
          Or p q -> combine p q
          Atm a -> fv a
    where combine p q = Tools.ListSet.union (fv p) (fv q)

instance Fv Sequent where
    fv (Sequent body head) = List.union (fv body) (fv head)
-- 
instance Fv a => Fv [a] where
  fv xs = Tools.ListSet.unions (map fv xs)
-- 

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
                                  Just v -> List.elem x (fv v)
                                  Nothing -> False) (fv p \\ [x]) 
               then variant x (fv(apply (Map.delete x env) p)) else x
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