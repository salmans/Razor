{- Razor
   Module      : Syntax.IFirstOrderUtils
   Description : Implements the functions in Syntax.FirstOrderUtils
   Maintainer  : Salman Saghafi -}


module Syntax.IFirstOrderUtils where

-- Standard
import Data.List (union, (\\))
import qualified Data.Map as Map

-- Control
import Control.Monad
import Control.Applicative

-- Syntax
import Syntax.Term
import Syntax.FirstOrder

-- Tools
import Tools.Utils (unions)


--------------------------------------------------------------------------------
-- Simplifying Formulas

{-| Simplifies the input first-order formula -}
simplify :: Formula -> Formula
simplify (Not p)        = simplify_helper $ Not $ simplify p
simplify (And p q)      = simplify_helper $ And (simplify p) (simplify q)
simplify (Or p q)       = simplify_helper $ Or (simplify p) (simplify q)
simplify (Imp p q)      = simplify_helper $ Imp (simplify p) (simplify q)
simplify (Iff p q)      = simplify_helper $ Iff (simplify p) (simplify q)
simplify (Forall x p)   = simplify_helper $ Forall x (simplify p)
simplify (Exists f x p) = simplify_helper $ Exists f x (simplify p)
simplify fm = fm

{- A helper for 'simplify' that defines cases for simplification. -}
simplify_helper :: Formula -> Formula
simplify_helper (Not Fls)         = Tru
simplify_helper (Not Tru)         = Fls
simplify_helper (And _ Fls)       = Fls
simplify_helper (And Fls _)       = Fls
simplify_helper (And Tru f)       = f
simplify_helper (And f Tru)       = f
simplify_helper (Or  Fls f)       = f
simplify_helper (Or  f Fls)       = f
simplify_helper (Or  Tru f)       = Tru
simplify_helper (Or  f Tru)       = Tru
simplify_helper (Imp Fls _)       = Tru
simplify_helper (Imp Tru f)       = f
simplify_helper (Imp _ Tru)       = Tru
simplify_helper (Imp f Fls)       = Not f
simplify_helper (Iff Tru f)       = f
simplify_helper (Iff f Tru)       = f
simplify_helper (Iff Fls f)       = Not f
simplify_helper (Iff f Fls)       = Not f
simplify_helper f@(Exists _ x f') = if elem x (freeVars f') then f else f'
simplify_helper f@(Forall x f')   = if elem x (freeVars f') then f else f'
simplify_helper f                 = f   -- otherwise
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
    freeVars Tru            = []
    freeVars Fls            = []
    freeVars (And f1 f2)    = (freeVars f1) `union` (freeVars f2)
    freeVars (Not f)        = freeVars f
    freeVars (Or  f1 f2)    = (freeVars f1) `union` (freeVars f2)
    freeVars (Imp f1 f2)    = (freeVars f1) `union` (freeVars f2)
    freeVars (Iff f1 f2)    = (freeVars f1) `union` (freeVars f2)
    freeVars (Atm a)        = freeVars a
    freeVars (Exists _ x f) = (freeVars f) \\ [x]
    freeVars (Forall x f)   = (freeVars f) \\ [x]

    constants Tru              = []
    constants Fls              = []
    constants (And f1 f2)      = (constants f1) `union` (constants f2)
    constants (Or  f1 f2)      = (constants f1) `union` (constants f2)
    constants (Imp  f1 f2)     = (constants f1) `union` (constants f2)
    constants (Iff  f1 f2)     = (constants f1) `union` (constants f2)
    constants (Atm a)          = constants a
    constants (Exists _ x f)   = constants f
    constants (Forall x f)     = constants f

    functionSyms Tru            = []
    functionSyms Fls            = []
    functionSyms (Atm a)        = functionSyms a
    functionSyms (Not f)        = functionSyms f
    functionSyms (And f1 f2)    = (functionSyms f1) `union` (functionSyms f2)
    functionSyms (Or  f1 f2)    = (functionSyms f1) `union` (functionSyms f2)
    functionSyms (Imp f1 f2)    = (functionSyms f1) `union` (functionSyms f2)
    functionSyms (Iff f1 f2)    = (functionSyms f1) `union` (functionSyms f2)
    functionSyms (Exists _ x f) = functionSyms f
    functionSyms (Forall x f)   = functionSyms f

    substitute _ Tru = Tru
    substitute _ Fls = Fls
    substitute env (Atm a)        = Atm $ substitute env a
    substitute env (Not p)        = Not $ substitute env p
    substitute env (And p q)      = And (substitute env p) (substitute env q)
    substitute env (Or p q)       = Or  (substitute env p) (substitute env q)
    substitute env (Imp p q)      = Imp (substitute env p) (substitute env q)
    substitute env (Iff p q)      = Iff (substitute env p) (substitute env q)
    substitute env (Exists f x p) = subquant env (Exists f) x p
    substitute env (Forall x p)   = subquant env Forall x p

    substituteConstants _ Tru              = Tru
    substituteConstants _ Fls              = Fls
    substituteConstants env (Atm a)        = Atm $ substituteConstants env a
    substituteConstants env (And p q)      = 
        And (substituteConstants env p) (substituteConstants env q)
    substituteConstants env (Or p q)       = 
        Or  (substituteConstants env p) (substituteConstants env q)
    substituteConstants env (Imp p q)      = 
        Imp  (substituteConstants env p) (substituteConstants env q)
    substituteConstants env (Iff p q)      = 
        Iff  (substituteConstants env p) (substituteConstants env q)
    substituteConstants env (Exists f x p) = 
        Exists f x $ substituteConstants env p
    substituteConstants env (Forall x p)   = 
        Forall x $ substituteConstants env p

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
--------------------------------------------------------------------------------
-- RelationBase class and instances
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