{- This file is part of Razor.

  Razor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Razor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Razor.  If not, see <http://www.gnu.org/licenses/>.

  Module      : Tools.IFolToGeo
  Description : The module implements functions to convert first-order formulas
  to geometric formulas.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Tools.IFolToGeo where

import Data.List
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad.Fix

import Syntax.Term
import qualified Syntax.GeometricUtils as Geo
import qualified Syntax.FirstOrderUtils as Fol
-- import qualified Utils.FolUtilities as FolU (freeVars, variant, apply)

{-| Parses an input string to construct an FOL formula, then users 
  parseFolToSequent to return a set of geometric sequents. -}
parseFolToTheory :: Bool -> String -> Maybe Geo.Theory
parseFolToTheory iscjr = (formulaToTheory iscjr).Fol.parseFormula

{-| formulaToTheory is the main function of the module, which may return 
  Just [Sequent] if the input formula is equivalent to a set of geometric 
  sequents, or Nothing otherwise. 
  If the input formula is a conjecrue (specified by the boolean flag) the 
  formula is guaranteed to be returned as a single sequent (disjunctions on 
  left won't be splitted). -}
formulaToTheory :: Bool -> Fol.Formula -> Maybe Geo.Theory
formulaToTheory iscjr fmla = 
    (\(l, r) -> let l' = normalizeAnd' l                          
                    ls = if iscjr 
                         then [l']
                         else splitDisjuncts l'
                in  (\l'' -> Geo.Sequent l'' r) <$> ls)
    <$> (formulaAllClauses.normalize) fmla
    where splitDisjuncts (Geo.Or p q) = splitDisjuncts p ++ splitDisjuncts q
          splitDisjuncts f            = [f]  -- otherwise

{- Just like formulaClauses but allows the formula to start with universal 
   quantifiers. However, since we treat free variables as if they are 
   universally quantified, the quantifiers are optional. -}
formulaAllClauses :: Fol.Formula -> Maybe (Geo.Formula, Geo.Formula)
formulaAllClauses (Fol.Forall x f) = (formulaAllClauses f) <|> formulaClauses f
formulaAllClauses f                = formulaClauses f

formulaClauses :: Fol.Formula -> Maybe (Geo.Formula, Geo.Formula)
formulaClauses (Fol.Or p q) = mergeFunc <$> 
                              formulaClauses p <*> formulaClauses q
    where mergeFunc (lp, rp) (lq, rq) = (Geo.simplify (Geo.And lp lq)
                                        , Geo.simplify (Geo.Or rp rq))

formulaClauses f = retFunc <$> formulaToClause f
    where retFunc (Left f')  = (f', Geo.Fls)
          retFunc (Right f') = (Geo.Tru, f')

formulaToClause :: Fol.Formula -> Maybe (Either Geo.Formula Geo.Formula)
formulaToClause (Fol.Not f@(Fol.Tru))   = Left <$> convertFormula f
formulaToClause (Fol.Not f@(Fol.Atm a)) = Left <$> convertFormula f
  -- Negate the clauses that will appear on the left of a sequent.
formulaToClause Fol.Fls            = Just $ Right Geo.Fls
formulaToClause f@(Fol.And p q)     = clauses <|> clause
    where clauses = do
            p' <- formulaToClause p
            q' <- formulaToClause q
            case (p', q') of
              (Left p'', Left q'') -> return $ Left $ Geo.Or p'' q''
              otherwise            -> Nothing
          clause = Right <$> formulaToConjunct f
formulaToClause (Fol.Exists fn x f) = do
  f' <- formulaToClause f
  case f' of
    Right f'' -> return $ Right $ Geo.Exists fn x f''
    otherwise -> Nothing
formulaToClause f               = do
  f' <- formulaToConjunct f
  return $ Right f'

formulaToConjunct :: Fol.Formula -> Maybe Geo.Formula
formulaToConjunct Fol.Fls       = Just Geo.Fls
formulaToConjunct (Fol.And p q) = Geo.And <$> formulaToConjunct p 
                                  <*> formulaToConjunct q
formulaToConjunct f@(Fol.Atm _) = convertFormula f
formulaToConjunct f         = Nothing -- otherwise

{- Converts an FOL formula structure to a GL formula structure. The function 
   returns Nothing if the input formula uses a data constructor that is not 
   defined for GL formulas (i.e., Not, Imp, Iff, Forall). -}
convertFormula :: Fol.Formula -> Maybe Geo.Formula
convertFormula (Fol.Tru)           = Just Geo.Tru
convertFormula (Fol.Fls)           = Just Geo.Fls
convertFormula (Fol.Atm a)         = Geo.Atm <$> convertAtom a
convertFormula (Fol.Or  p q)       = Geo.Or <$> convertFormula p <*> 
                                     convertFormula q
convertFormula (Fol.And p q)       = Geo.Or <$> convertFormula p <*> 
                                     convertFormula q
convertFormula (Fol.Exists fn v p) = Geo.Exists fn v <$> convertFormula p
convertFormula _                   = Nothing -- otherwise

{-| This funciton is used to convert an FOL term data structure to a Geo term 
  data structure. -}
convertAtom :: Fol.Atom -> Maybe Geo.Atom
convertAtom (Fol.Rel s ts)   = return $ Geo.Rel s ts
convertAtom (Fol.FnRel s ts) = return $ Geo.FnRel s ts

normalize :: Fol.Formula -> Fol.Formula
normalize = normalize'.pnfAll.elimImp
    where normalize' = fix $ \func x -> let y = (pushExists.pushAnd.nnf) x
                                                -- Salman: is nnf redundant?
                                        in  if x == y then y else func y

{- We need this function since after construction of a sequent, we may end up 
   with disjunction over conjunction. -}
normalizeAnd' :: Geo.Formula -> Geo.Formula
normalizeAnd' = fix $ \func x -> let y = pushAnd' x
                                 in  if x == y then y else func y
                                    -- Salman: do we need fix?

{- pushAnd for geometric formulas -}
pushAnd' :: Geo.Formula -> Geo.Formula
pushAnd' (Geo.And r (Geo.Or p q)) = Geo.Or (pushAnd' (Geo.And r p)) 
                                          (pushAnd' (Geo.And r q))
pushAnd' (Geo.And (Geo.Or p q) r) = Geo.Or (pushAnd' (Geo.And p r)) 
                                          (pushAnd' (Geo.And q r))
pushAnd' (Geo.And p q)        = 
    case (pushAnd' p, pushAnd' q) of -- inside-out processing
      (Geo.Or p' p'', q') -> Geo.Or (Geo.And p' q') (Geo.And p'' q')
      (p', Geo.Or q' q'') -> Geo.Or (Geo.And p' q') (Geo.And p' q'')
      (p', q')        -> Geo.And p' q'
pushAnd' (Geo.Or  p q)        = Geo.Or  (pushAnd' p) (pushAnd' q)
pushAnd' f = f -- otherwise


-- Implication Elimination
elimImp :: Fol.Formula -> Fol.Formula
elimImp (Fol.Imp p q)       = Fol.Or (Fol.Not (elimImp p)) (elimImp q)
elimImp (Fol.Iff p q)       = Fol.And (Fol.Or (Fol.Not (elimImp p)) (elimImp q))
                              (Fol.Or (Fol.Not (elimImp q)) (elimImp p))
elimImp (Fol.Not p)         = Fol.Not (elimImp p)
elimImp (Fol.And p q)       = Fol.And (elimImp p) (elimImp q)
elimImp (Fol.Or p q)        = Fol.Or  (elimImp p) (elimImp q)
elimImp (Fol.Forall x p)    = Fol.Forall x (elimImp p)
elimImp (Fol.Exists fn x p) = Fol.Exists fn x (elimImp p)
elimImp f = f  -- otherwise

{- Prenex Universal Quantifiers -}
pnfAll :: Fol.Formula -> Fol.Formula
pnfAll = prenexAll . nnf . Fol.simplify
-- 
prenexAll :: Fol.Formula -> Fol.Formula
prenexAll (Fol.Forall x p) = Fol.Forall x (prenexAll p)
-- prenexAll (Fol.Exists x p) = Fol.Exists x (prenexAll p)
prenexAll (Fol.Exists fn x p) = pullquants (Fol.Exists fn x (prenexAll p))
prenexAll (Fol.And p q)       = pullquants (Fol.And (prenexAll p) (prenexAll q))
prenexAll (Fol.Or p q)        = pullquants (Fol.Or (prenexAll p) (prenexAll q))
prenexAll fm = fm
-- 
pullquants :: Fol.Formula -> Fol.Formula
pullquants fm = case fm of
 Fol.And (Fol.Forall x p) (Fol.Forall y q) -> 
     pullq (True,True) fm Fol.Forall Fol.And x y p q
 Fol.And (Fol.Forall x p) q                -> 
     pullq (True,False) fm Fol.Forall Fol.And x x p q
 Fol.And p (Fol.Forall y q)                -> 
     pullq (False,True) fm Fol.Forall Fol.And y y p q
 Fol.Or (Fol.Forall x p) q                 ->  
     pullq (True,False) fm Fol.Forall Fol.Or x x p q
 Fol.Or p (Fol.Forall y q)                 -> 
     pullq (False,True) fm Fol.Forall Fol.Or y y p q
 Fol.Exists fn x (Fol.Forall y p)          -> 
     pullq' True fm Fol.Forall (Fol.Exists fn x) y p
 _ -> fm

pullq :: (Bool, Bool) -> Fol.Formula
     -> (Fol.Variable -> Fol.Formula -> Fol.Formula)
     -> (Fol.Formula -> Fol.Formula -> Fol.Formula) 
     -> Fol.Variable -> Fol.Variable
     -> Fol.Formula -> Fol.Formula -> Fol.Formula
pullq (l,r) fm quant op x y p q =
 let z = Fol.variant x (freeVars fm) 
     p' = if l then substitute (Map.singleton x (Fol.Var z)) p else p
     q' = if r then substitute (Map.singleton y (Fol.Var z)) q else q in
 quant z (pullquants(op p' q'))

pullq' :: Bool -> Fol.Formula
     -> (Variable -> Fol.Formula -> Fol.Formula)
     -> (Fol.Formula -> Fol.Formula) -> Variable
     -> Fol.Formula -> Fol.Formula
pullq' b fm quant op x p =
 let z = variant x (freeVars fm) 
     p' = if b then substitute (Map.singleton x (Fol.Var z)) p else p
 in  quant z (pullquants (op p'))

-- 
-- Converts a formula to negation normal form
nnf :: Fol.Formula -> Fol.Formula
nnf (Fol.And p q)                 = Fol.And (nnf p) (nnf q)
nnf (Fol.Or p q)                  = Fol.Or (nnf p) (nnf q)
nnf (Fol.Imp p q)                 = Fol.Or (nnf (Fol.Not p)) (nnf q)
nnf (Fol.Iff p q)                 = Fol.Or (Fol.And (nnf p) (nnf q)) 
                                    (Fol.And (nnf(Fol.Not p)) (nnf(Fol.Not q)))
nnf (Fol.Not(Fol.Not p))          = nnf p
nnf (Fol.Not(Fol.And p q))        = Fol.Or (nnf(Fol.Not p)) (nnf(Fol.Not q))
nnf (Fol.Not(Fol.Or p q))         = Fol.And (nnf(Fol.Not p)) (nnf(Fol.Not q))
nnf (Fol.Not(Fol.Imp p q))        = Fol.And (nnf p) (nnf(Fol.Not q))
nnf (Fol.Not(Fol.Iff p q))        = Fol.Or (Fol.And (nnf p) (nnf(Fol.Not q))) 
                                    (Fol.And (nnf(Fol.Not p)) (nnf q))
nnf (Fol.Forall x p)              = Fol.Forall x (nnf p)
nnf (Fol.Exists fn x p)           = Fol.Exists fn x (nnf p)
nnf (Fol.Not (Fol.Forall x p))    = Fol.Exists Nothing x (nnf (Fol.Not p))
nnf (Fol.Not (Fol.Exists fn x p)) = Fol.Forall x (nnf (Fol.Not p))
nnf fm = fm

pushExists :: Fol.Formula -> Fol.Formula
pushExists (Fol.Not f)         = Fol.Not (pushExists f)
pushExists (Fol.And p q)       = Fol.And (pushExists p) (pushExists q)
pushExists (Fol.Or p q)        = Fol.Or (pushExists p) (pushExists q)
pushExists (Fol.Exists fn x f) = 
    case pushExists f of -- inside-out processing
      Fol.Or p q -> Fol.Or (Fol.Exists fn x p) (Fol.Exists fn x q)
      f          -> Fol.Exists fn x f
pushExists (Fol.Forall x f)    = Fol.Forall x (pushExists f)
pushExists f                   = f -- otherwise

pushAnd :: Fol.Formula -> Fol.Formula
pushAnd (Fol.And r (Fol.Or p q)) = Fol.Or (pushAnd (Fol.And r p)) 
                                          (pushAnd (Fol.And r q))
pushAnd (Fol.And (Fol.Or p q) r) = Fol.Or (pushAnd (Fol.And p r)) 
                                          (pushAnd (Fol.And q r))
pushAnd (Fol.Not f)          = Fol.Not (pushAnd f)
pushAnd (Fol.And p q)        = 
    case (pushAnd p, pushAnd q) of -- inside-out processing
      (Fol.Or p' p'', q') -> Fol.Or (Fol.And p' q') (Fol.And p'' q')
      (p', Fol.Or q' q'') -> Fol.Or (Fol.And p' q') (Fol.And p' q'')
      (p', q')        -> Fol.And p' q'
pushAnd (Fol.Or  p q)        = Fol.Or  (pushAnd p) (pushAnd q)
pushAnd (Fol.Forall x p)     = Fol.Forall x (pushAnd p)
pushAnd (Fol.Exists fn x p)  = Fol.Exists fn x (pushAnd p)
pushAnd f = f -- otherwise