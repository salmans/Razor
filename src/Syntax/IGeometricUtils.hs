{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DoAndIfThenElse #-}

{- Razor
   Module      : Syntax.IGeometricUtils
   Description : Implements the functions in Syntax.GeometricUtils
   Maintainer  : Salman Saghafi -}


module Syntax.IGeometricUtils where

-- Standard
import Data.List (nub, union, (\\))
import qualified Data.Map as Map
import Data.Maybe

-- Common
import Common.Basic

-- Control
import Control.Monad
import Control.Applicative
import Control.Monad.State.Lazy as State

-- Syntax
import Syntax.Term
import Syntax.Geometric

-- Tools
import Tools.Utils (unions)
import Tools.Counter (Counter, increment)
import Tools.Trace


--------------------------------------------------------------------------------
-- Simplifying Formulas
{-| Simplifies an input geometric 'Formula', if possible. -}
simplify :: Formula -> Formula
simplify (And p q)        = simplify_helper $ And (simplify p) (simplify q)
simplify (Or p q)         = simplify_helper $ Or (simplify p) (simplify q)
simplify (Exists fn x p)  = simplify_helper $ Exists fn x (simplify p)
simplify (Lone fn x p lf) = simplify_helper $ Lone fn x (simplify p) lf
simplify fm = fm

{- A helper for 'simplify' that defines cases for simplification. -}
simplify_helper :: Formula -> Formula
simplify_helper (And _ Fls)        = Fls
simplify_helper (And Fls _)        = Fls
simplify_helper (And Tru f)        = f
simplify_helper (And f Tru)        = f
simplify_helper (Or  Fls f)        = f
simplify_helper (Or  f Fls)        = f
simplify_helper (Or  Tru f)        = Tru
simplify_helper (Or  f Tru)        = Tru
simplify_helper f@(Exists fn x f') = if elem x (freeVars f') then f else f'
simplify_helper f@(Lone fn x f' _) = if elem x (freeVars f') then f else f'
simplify_helper f                  = f   -- otherwise
--------------------------------------------------------------------------------
-- Converting sequents with function symbols to purely relational sequents
preprocess :: Theory -> Theory
preprocess = (addElementPreds <$>).linearize.relationalize

{-| Eliminates all function symbols in the sequents of a theory and replaces 
  them with relational symbols. It also adds a set of additional sequents to 
  enforce functional integrity and a set of axioms to enforce correctness on
  equality relation. -}
relationalize :: Theory -> Theory
relationalize thy  = 
    (fst $ State.runState (relationalizeTheory thy) 0) -- ++ integ ++ eqAxs
    where funcs  = filter (\fa -> snd fa /= 0) $ functionSyms thy
          funcs' = (\(f, a) -> (f, a + 1)) <$> funcs
                   -- arity of functions as relations increases by one
          rels   = relationSyms thy
          integ  = integritySequents <$> funcs
          eqAxs  = equivalenceAxioms ++
                   concatMap (uncurry relationCongruenceAxioms) rels ++ 
                   concatMap (uncurry functionCongruenceAxioms) funcs'


-- Lone Nothing (Variable "x") (parseFormula "a(x) & x = y") (parseFormula "Truth")
linearize :: Theory -> Theory
linearize thy = (\(Sequent bdy hd) -> Sequent (linearizeFormula bdy) hd) <$> thy

linearizeFormula :: Formula -> Formula
linearizeFormula fmla = 
    fst $ (linearizeFormulaHelper varMap) fmla
    where varMap = Map.fromList $ (\v -> (v, 0)) <$> freeVars fmla

linearizeFormulaHelper :: Map.Map Variable Int -> Formula -> 
                          (Formula, Map.Map Variable Int)
linearizeFormulaHelper varMap Tru                  = (Tru, varMap)
linearizeFormulaHelper varMap (Atm (FnRel sym terms)) = 
    let (varMap', terms') = 
            foldr (\(Var v@(Variable name)) (vm, ts)  -> 
                       let (val, vm') = getVarVal v vm
                           v'         = if   val == 0
                                        then v
                                        else Variable (name ++ ":" ++ (show val)) 
                       in  (vm', [Var v'] ++ ts)) (varMap, []) terms
        diffMap           = Map.differenceWith
                            (\[b] [a] -> if a == b
                                         then Nothing
                                         else if   a == 0
                                              then Just [(a+1)..(b-1)]
                                              else Just [a..(b-1)]) 
                            (Map.map pure varMap') 
                            (Map.map pure varMap)
        atm               = Atm (FnRel sym terms')
    in (Map.foldrWithKey (\key inds fmla ->
                              case makeAnd key inds of
                                Nothing -> fmla
                                Just fm -> And fmla fm) atm diffMap, varMap')
    where getVarVal v@(Variable name) vm = 
              let val = Map.findWithDefault 0 v vm
              in  (val, Map.insert v (val + 1) vm)
          makeAnd v@(Variable n) inds = 
              let eqs = (\i -> 
                             Atm $ Rel "=" [ Var v
                                           , Var $ Variable (n ++ ":" ++ (show i))]) <$> inds
              in  if null eqs then Nothing else Just $ foldr1 And eqs
linearizeFormulaHelper varMap a@(Atm (Rel "=" _))   = (a, varMap)
linearizeFormulaHelper varMap (Atm (Rel sym terms)) = 
    let (varMap', terms') = 
            foldr (\(Var v@(Variable name)) (vm, ts)  -> 
                       let (val, vm') = getVarVal v vm
                           v'         = if   val == 0
                                        then v
                                        else Variable (name ++ ":" ++ (show val)) 
                       in  (vm', [Var v'] ++ ts)) (varMap, []) terms
        diffMap           = Map.differenceWith
                            (\[b] [a] -> if a == b
                                         then Nothing
                                         else if   a == 0
                                              then Just [(a+1)..(b-1)]
                                              else Just [a..(b-1)]) 
                            (Map.map pure varMap') 
                            (Map.map pure varMap)
        atm               = Atm (Rel sym terms')
    in (Map.foldrWithKey (\key inds fmla ->
                              case makeAnd key inds of
                                Nothing -> fmla
                                Just fm -> And fmla fm) atm diffMap, varMap')
    where getVarVal v@(Variable name) vm = 
              let val = Map.findWithDefault 0 v vm
              in  (val, Map.insert v (val + 1) vm)
          makeAnd v@(Variable n) inds = 
              let eqs = (\i -> 
                             Atm $ Rel "=" [ Var v
                                           , Var $ Variable (n ++ ":" ++ (show i))]) <$> inds
              in  if null eqs then Nothing else Just $ foldr1 And eqs
linearizeFormulaHelper varMap (And fmla1 fmla2)    = 
    let (fmla1', varMap' ) = linearizeFormulaHelper varMap fmla1
        (fmla2', varMap'') = linearizeFormulaHelper varMap' fmla2
    in  (And fmla1' fmla2', varMap'')
linearizeFormulaHelper varMap (Lone sk x fmla unq) =
    let prevX    = Map.lookup x varMap                   
        (fmla', varMap') = linearizeFormulaHelper (Map.insert x 0 varMap) fmla
        varMap'' = case prevX of
                     Nothing  -> varMap'
                     Just val -> Map.insert x val varMap'
    in  (Lone sk x fmla' unq, varMap'')

{- Eliminates all function symbols in the 'Sequent's of a 'Theory' and replaces 
   them with relational symbols in the conventional way. -}
relationalizeTheory :: Theory -> Counter Theory
relationalizeTheory thy = mapM relationalizeSequent thy

{- Eliminates all function symbols in a 'Sequent' and replaces them with 
   relational symbols. -}
relationalizeSequent :: Sequent -> Counter Sequent
relationalizeSequent (Sequent bdy hd) = do
  bdyres <- relationalizeFormula bdy
  hdres  <- relationalizeFormula hd
  return $ Sequent (makeExists bdyres) (makeExists hdres)
    where makeExists (fmla, skvs) = 
              let (vs', fmla') = takeExistsOut fmla
              in putExistsBack vs'
                     $ foldl (\f (sk, lfmla, v)  -> 
                              case lfmla of
                                Nothing -> Exists sk v f
                                Just lf -> Lone sk v f lf) fmla' skvs

-------TAKE EXISTS OUT ----------
-- THIS IS A TEMPORARY SOLUTION
data VarTree = VarTreeLeaf [(Variable, Maybe FnSym)]
             | VarTreeNode VarTree VarTree
             deriving Show

takeExistsOut :: Formula -> (VarTree, Formula)
takeExistsOut Tru = (VarTreeLeaf [], Tru)
takeExistsOut Fls = (VarTreeLeaf [], Fls)
takeExistsOut a@(Atm _) = (VarTreeLeaf [], a)
takeExistsOut (And f1 f2) =
    -- No disjunction inside conjunction!
    let (VarTreeLeaf vs1, f1') = takeExistsOut f1
        (VarTreeLeaf vs2, f2') = takeExistsOut f2
    in  (VarTreeLeaf (vs1 ++ vs2), And f1' f2')
takeExistsOut (Or f1 f2) =
    let (vs1, f1') = takeExistsOut f1
        (vs2, f2') = takeExistsOut f2
    in  (VarTreeNode vs1 vs2, Or f1' f2')
takeExistsOut (Exists fn v f) =
    -- No disjunctions inside existentials
    let (VarTreeLeaf vs, f') = takeExistsOut f
    in  (VarTreeLeaf ((v, fn):vs), f')
takeExistsOut (Lone fn v f unq) = 
    let (vs, f') = takeExistsOut f
    in  (vs, Lone fn v f' unq)

putExistsBack :: VarTree -> Formula -> Formula
putExistsBack (VarTreeNode t1 t2) (Or f1 f2) =
    let f1' = putExistsBack t1 f1
        f2' = putExistsBack t2 f2
    in  Or f1' f2'
putExistsBack (VarTreeLeaf vs) f = foldr (\(v, fn) f -> Exists fn v f) f vs

--------------------------------



{- Eliminates all function symbols in a formula and replaces them with 
  relational symbols. It also retruns a list of skolem functions, unique 
  formulas and existential variable pairs introduced in this way. -}
relationalizeFormula :: Formula -> 
                        Counter ( Formula, [( Maybe FnSym
                                           , Maybe Formula
                                           , Variable)])
relationalizeFormula Tru                = return (Tru, [])
relationalizeFormula Fls                = return (Fls, [])
relationalizeFormula (Atm (Rel sym ts)) = do
  (fs', ts', vs') <- foldM foldFunc ([], [], []) ts 
  return $ (andFmlas fs' (Atm (Rel sym ts')), vs')
    -- Replace ts with new variables computed by flattening terms. Also, 
    -- return a formula, corresponding to the conjunction of the formulas for
    -- flattening subterms. (This is pretty much like flattening terms below.)
    where andFmlas  = \fmlas fmla -> foldr (\f1 f2 -> And f1 f2) fmla fmlas
          foldFunc  = \(ffs, fts, fvs) t -> 
              do
                res <- relationalizeTerm t
                case res of
                  Nothing            -> 
                      return (ffs, fts ++ [t], fvs)
                  Just (tf, tt, tvs) -> 
                      return (ffs ++ [tf], fts ++ [tt], fvs ++ tvs)
relationalizeFormula (And fmla1 fmla2)  = do
  (fmla1', vs1) <- relationalizeFormula fmla1
  (fmla2', vs2) <- relationalizeFormula fmla2
  return $ (And fmla1' fmla2', vs1 ++ vs2)
relationalizeFormula (Or fmla1 fmla2)   = do
  res1 <- relationalizeFormula fmla1
  res2 <- relationalizeFormula fmla2
  return $ (Or (makeExists res1) (makeExists res2), [])
      -- Since disjunctions occur at the topmost level, apply existential
      -- quantification on the newly introduced variables before returning from
      -- this call.
  where makeExists (fmla, skvs) = 
            foldl (\f (sk, lfmla, v) -> 
                       case lfmla of
                         Nothing -> Exists sk v f
                         Just lf -> Lone sk v f lf) fmla skvs
relationalizeFormula (Exists fn v fmla)    = do
  (fmla', vs) <- relationalizeFormula fmla
  if isJust fn
  then return $ (Exists fn v fmla', vs)
  else do 
    fn' <- freshSymbol "exists"
    return $ (Exists (Just fn') v fmla', vs)
{- Eliminates all function symbols in a term and replaces them with relational 
   symbols. It also returns a set of skolem functions, unique formulas, and 
   existential variable pairs introduced by the process. -}
relationalizeTerm :: Term -> 
                     Counter (Maybe ( Formula
                                    , Term
                                    , [(Maybe FnSym, Maybe Formula, Variable)]))
relationalizeTerm (Var v)   = return Nothing
relationalizeTerm (Cons (Constant c))  
                            = relationalizeTerm (Fn c [])
                            -- functions as nullary functions
-- relationalizeTerm (Fn _ []) = return Nothing 
relationalizeTerm (Fn f ts) = do
  (fs', ts', skvs') <- foldM foldFunc ([], [], []) ts
  v               <- freshVariable
  let var         =  Var v
  ind <- increment -- get an index to add to the corresponding skolem function
  return $ Just (andFmlas fs' (Atm (FnRel f (ts' ++ [var])))
                , var
                , ( Just $ f ++ "#" ++ (show ind)
                  , Just $ Atm (FnRel f (ts' ++ [var]))
                  , v):skvs')
    where andFmlas  = \fmlas fmla -> foldr (\f1 f2 -> And f1 f2) fmla fmlas
          foldFunc  = \(ffs, fts, fvs) t -> -- for each subterm
              do
                res <- relationalizeTerm t -- flatten the subterm
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

{- Create additional sequents to enforce function integrity constraints. -}
integritySequents :: (FnSym, Int) -> Sequent
integritySequents fs = Sequent (lft fs) (parseFormula "y = y'")
    where lft (f, a) = And (Atm (FnRel f (vars a ++ [Var (Variable "y") ])))
                           (Atm (FnRel f (vars a ++ [Var (Variable "y'")])))
          vars a     = (\i -> Var (Variable ("x" ++ show i))) <$> [1.. a]

{- Adds three axioms to enforce symmetric, reflexive and transitive properties 
   of equality:
     1. @x = x@ 
     2. @x = y => y = x@ 
     3. @x = y & y = z => x = z@ -}
equivalenceAxioms :: Theory
equivalenceAxioms =  parseSequent <$> [ "x = x"
                                      , "x = y => y = x"
                                      , "x = y & y = z => x = z"]

{- For every relation @R@ of arity @n@ in the theory, adds axioms in the form 
   @R(x1, x2, ..., xn) & x1 = x1' => R(x1', x2, ..., xn)@ -}
relationCongruenceAxioms :: RelSym -> Int -> Theory
relationCongruenceAxioms "=" _     = [] -- don't do anything for equality
relationCongruenceAxioms sym arity = 
    let var      = Var . Variable
        vars i j = (\i -> var ("x" ++ (show i))) <$> [i..j]
        seq i    = Sequent (And (Atm $ Rel sym $ vars 1 arity)
                                (Atm $ Rel "=" [ var $ "x" ++ (show i)
                                               , var "y"]))
                           (Atm $ Rel sym $ (vars 1 (i - 1) ++ 
                                            (var "y"):(vars (i + 1) arity)))
    in  seq <$> [1 .. arity]

{- For every functional relation @F@ of arity @n@ in the theory, adds axioms in 
   the form @F(x1, x2, ..., xn) & x1 = x1' => F(x1', x2, ..., xn)@ -}
functionCongruenceAxioms :: FnSym -> Int -> Theory
functionCongruenceAxioms "=" _     = [] -- don't do anything for equality
functionCongruenceAxioms sym arity = 
    let var      = Var . Variable
        vars i j = (\i -> var ("x" ++ (show i))) <$> [i..j]
        seq i    = Sequent (And (Atm $ FnRel sym $ vars 1 arity)
                                (Atm $ Rel "=" [ var $ "x" ++ (show i)
                                                 , var "y"]))
                           (Atm $ FnRel sym $ (vars 1 (i - 1) ++ 
                                              (var "y"):(vars (i + 1) arity)))
    in  seq <$> [1 .. arity]
--------------------------------------------------------------------------------
-- Adding @Element predicates to deal with free variables in the head of 
-- sequents

{-| The current implementation of the Chase requires every free variable in the
  head of a 'Sequent' to be defined in its body. Thus, we enrich the theory with
  a new predicate, @Element, and make @Element true for every free variable
  in the head of a 'Sequent' that is not been defined in the body. We also add
  @Element to the right of every sequent with existential quantifiers. -}
addElementPreds :: Sequent -> Sequent
addElementPreds seq = 
    let bdy' = foldr (\v b -> andFunc (Atm (elementPred (Var v))) b) bdy hdFVars
        hd'  = addElementPredToRight [] hd
    in  seq { sequentBody = bdy' 
            , sequentHead = hd' }
    where hd      = sequentHead seq
          bdy     = sequentBody seq
          hdVars  = freeVars hd
          bdyVars = freeVars bdy
          hdFVars = hdVars \\ bdyVars
          allVars = hdVars `union` bdyVars
          elementPred = \x -> Rel "@Element" [x]
          andFunc  Tru x = x
          andFunc  x Tru = x
          andFunc  x y   = And x y


{- A helper for addElementPred, which adds @Element predicate to the right of 
   a 'Sequent'. The initial parameter of a list of variables is used to stack up
   all existentially quantified variables and add them in the most-inner level
   of the formula when the inductive steps are over.

   NOTE: The function assumes the input 'Sequent' to be in the standard form, 
   where disjunction only appears at the topmost level in the head and 
   existentials only appear in the head outside of conjunctions. Also, the 
   function assumes that the sequent has been flattened before it is passed to
   this function.
 -}
addElementPredToRight :: [Variable] -> Formula -> Formula
addElementPredToRight _ (Or fmla1 fmla2) = -- go inside disjunction
    Or (addElementPredToRight [] fmla1) (addElementPredToRight [] fmla2)
    -- this assumes standard sequents
addElementPredToRight vs (And fmla1 fmla2) = 
    And (addElementPredToRight [] fmla1) (addElementPredToRight vs fmla2)
addElementPredToRight vs (Exists fn x fmla)  = -- add predicate for existentials
    Exists fn x (addElementPredToRight (x:vs) fmla)
addElementPredToRight vs (Lone fn x fmla lf) = -- add predicate for existentials
    Lone fn x (addElementPredToRight (x:vs) fmla) lf
addElementPredToRight vs fmla@(Atm atm@(Rel sym ts)) =
    let preds = (elementPred <$> ts') ++ ((elementPred.Var) <$> vs)
    in  foldr (\p f -> And (Atm p) f) fmla preds
    -- this assumes flattened terms
    where ts' = filter (not.isVariable) ts
addElementPredToRight _ fmla = fmla -- again, the sequent is in standard form


{- A helper for addElementPred. It constructs an @Element predicate for the 
   input term. -}
elementPred :: Term -> Atom
elementPred =  \x -> Rel "@Element" [x]


{- Returns true if the input 'Sequent' has any free variable in its head, which
  is not defined on its body. -}
hasFreeHeadVar :: Sequent -> Bool
hasFreeHeadVar seq = (not.null) $ hdVars \\ bdyVars
    where hd      = sequentHead seq
          bdy     = sequentBody seq
          hdVars  = freeVars hd
          bdyVars = freeVars bdy

{-| Given an input geometric 'Sequent', returns a list of Skolem functions for
  existential quantifiers in the sequent. -}
sequentExistentials :: Sequent -> [FnSym]
sequentExistentials (Sequent bdy hd) =  
    formulaExistentials bdy ++ formulaExistentials hd

{-| Given an input geometric 'Formula', returns a list of Skolem functions for 
  its existential quantifiers.-}
formulaExistentials :: Formula -> [FnSym]
formulaExistentials Tru                    = []
formulaExistentials Fls                    = []
formulaExistentials (And f1 f2)            = 
    formulaExistentials f1 ++ formulaExistentials f2
formulaExistentials (Or f1 f2)             = 
    formulaExistentials f1 ++ formulaExistentials f2
formulaExistentials (Atm a)                = []
formulaExistentials (Exists (Just fn) _ f) = fn:(formulaExistentials f)
formulaExistentials (Exists Nothing _ f)   = formulaExistentials f
formulaExistentials (Lone (Just fn) _ f _) = fn:(formulaExistentials f)
formulaExistentials (Lone Nothing _ f _)   = formulaExistentials f

{-| Applies an existential substitution of type 'ExistsSub' to a 'Sequent'. -}
sequentExistsSubstitute :: ExistsSub -> Sequent -> Sequent
sequentExistsSubstitute env (Sequent bdy hd) =
    Sequent (formulaExistsSubstitute env bdy) (formulaExistsSubstitute env hd)

{- A helper for 'sequentExistsSubstitution' to apply a substitution of type 
   'ExistsSub' to a 'Formula'. -}
formulaExistsSubstitute :: ExistsSub -> Formula -> Formula
formulaExistsSubstitute _ Tru               = Tru
formulaExistsSubstitute _ Fls               = Fls
formulaExistsSubstitute env a@(Atm _)       = a
formulaExistsSubstitute env (And f1 f2)     = 
    And (formulaExistsSubstitute env f1) (formulaExistsSubstitute env f2)
formulaExistsSubstitute env (Or f1 f2)      = 
    Or (formulaExistsSubstitute env f1) (formulaExistsSubstitute env f2)
formulaExistsSubstitute env (Exists fn@(Just fnSym) x f) = 
    case Map.lookup fnSym env of
      Just t  -> let f' = substitute (Map.singleton x t) f
                 in  formulaExistsSubstitute env f'
      Nothing -> Exists fn x $ formulaExistsSubstitute env f
formulaExistsSubstitute env (Exists Nothing x f)   = 
    Exists Nothing x $ formulaExistsSubstitute env f
formulaExistsSubstitute env (Lone fn@(Just fnSym) x f lf) = 
    case Map.lookup fnSym env of
      Just t  -> let f' = substitute (Map.singleton x t) f
                 in  formulaExistsSubstitute env f'
      Nothing -> Lone fn x (formulaExistsSubstitute env f) lf
formulaExistsSubstitute env (Lone Nothing x f lf)   = 
    Lone Nothing x (formulaExistsSubstitute env f) lf

--------------------------------------------------------------------------------
-- TermBased class and some implementations

{- Atom is TermBased -}
instance TermBased Atom where
    freeVars (Rel   _ args)   = unions (freeVars <$> args)
    freeVars (FnRel _ args)   = unions (freeVars <$> args)

    constants (Rel _ args)    = unions (constants <$> args)
    constants (FnRel _ args)  = unions (constants <$> args)

    functionSyms (Rel _ args)   = foldr (union.functionSyms) [] args
    functionSyms (FnRel _ args) = foldr (union.functionSyms) [] args

    substitute env (Rel r args)   = Rel   r $ (substitute env) <$> args
    substitute env (FnRel r args) = FnRel r $ (substitute env) <$> args

    substituteConstants env (Rel r args)   = 
        Rel r $ (substituteConstants env) <$> args
    substituteConstants env (FnRel r args) = 
        FnRel r $ (substituteConstants env) <$> args

{- Formula is TermBased -}
instance TermBased Formula where
    freeVars Tru              = []
    freeVars Fls              = []
    freeVars (And f1 f2)      = (freeVars f1) `union` (freeVars f2)
    freeVars (Or  f1 f2)      = (freeVars f1) `union` (freeVars f2)
    freeVars (Atm a)          = freeVars a
    freeVars (Exists _ x f)   = (freeVars f) \\ [x]
    freeVars (Lone _ x f _)   = (freeVars f) \\ [x]

    constants Tru              = []
    constants Fls              = []
    constants (And f1 f2)      = (constants f1) `union` (constants f2)
    constants (Or  f1 f2)      = (constants f1) `union` (constants f2)
    constants (Atm a)          = constants a
    constants (Exists _ x f)   = constants f
    constants (Lone _ x f _)   = constants f

    functionSyms Tru            = []
    functionSyms Fls            = []
    functionSyms (Atm a)        = functionSyms a
    functionSyms (And f1 f2)    = (functionSyms f1) `union` (functionSyms f2)
    functionSyms (Or  f1 f2)    = (functionSyms f1) `union` (functionSyms f2)
    functionSyms (Exists _ x f) = functionSyms f
    functionSyms (Lone _ x f _) = functionSyms f

    substitute _ Tru               = Tru
    substitute _ Fls               = Fls
    substitute env (Atm a)         = Atm $ substitute env a
    substitute env (And p q)       = And (substitute env p) (substitute env q)
    substitute env (Or p q)        = Or  (substitute env p) (substitute env q)
    substitute env (Exists f x p)  = subquant env (Exists f) x p
    substitute env (Lone f x p lf) = subquant env (\v u -> Lone f v u lf) x p

    substituteConstants _ Tru              = Tru
    substituteConstants _ Fls              = Fls
    substituteConstants env (Atm a)        = Atm $ substituteConstants env a
    substituteConstants env (And p q)      = 
        And (substituteConstants env p) (substituteConstants env q)
    substituteConstants env (Or p q)       = 
        Or  (substituteConstants env p) (substituteConstants env q)
    substituteConstants env (Exists f x p) = 
        Exists f x $ substituteConstants env p
    substituteConstants env (Lone f x p lf) = 
        Lone f x (substituteConstants env p) lf


{- A helper for 'substitute' implementation for 'Formula', that applies a 
   substitution to a quantified formula. -}
subquant :: Sub -> (Variable -> Formula -> Formula) 
            -> Variable -> Formula -> Formula
subquant env quant x f = quant x' $ substitute ((Map.insert x (Var x')) env) f
    where x' = if any (\k -> case Map.lookup k env of
                                  Just v -> elem x (freeVars v)
                                  Nothing -> False) 
                      (freeVars f \\ [x]) 
               then variant x $ freeVars $ substitute (Map.delete x env) f
               else x
-- 
{- Sequent is TermBased -}
instance TermBased Sequent where
    freeVars (Sequent bdy hds)     = (freeVars bdy) `union` (freeVars hds)

    constants (Sequent bdy hds)    = (constants bdy) `union` (constants hds)

    functionSyms (Sequent bdy hds) = 
        (functionSyms bdy) `union` (functionSyms hds)

    substitute env (Sequent bdy hds) =
        Sequent (substitute env bdy) (substitute env hds)

    substituteConstants env (Sequent bdy hds) =
        Sequent (substituteConstants env bdy) (substituteConstants env hds)

{- Theory is TermBased -}
instance TermBased Theory where
    freeVars       = nub.(concatMap freeVars)
    
    constants      = nub.(concatMap constants)

    functionSyms   = nub.(concatMap functionSyms)

    substitute env = ((substitute env) <$>)

    substituteConstants env = ((substituteConstants env) <$>)
--------------------------------------------------------------------------------
-- RelationBased class and instances
{-| RelationBased is the class of types that are formed by relations or have 
  relations as a part of their definition, e.g. 'Formula' and 'Sequent':
  [@relationSyms@] returns a list of relation symbol and arity pairs, contained 
  in an instance of type @a@.
 -}
class RelationBased a where
    relationSyms :: a -> [(FnSym, Int)]


instance RelationBased Atom where
    relationSyms (Rel sym ts)   = [(sym, length ts)]
    relationSyms (FnRel sym ts) = [(sym, length ts)]

instance RelationBased Formula where
    relationSyms Tru            = []
    relationSyms Fls            = []
    relationSyms (Atm a)        = relationSyms a
    relationSyms (And p q)      = union (relationSyms p) (relationSyms q)
    relationSyms (Or p q)       = union (relationSyms p) (relationSyms q)
    relationSyms (Exists _ x p) = relationSyms p
    relationSyms (Lone _ x p _) = relationSyms p

instance RelationBased Sequent where
    relationSyms (Sequent bdy hd) = union (relationSyms bdy) (relationSyms hd)

instance RelationBased Theory where
    relationSyms = nub.(concatMap relationSyms)