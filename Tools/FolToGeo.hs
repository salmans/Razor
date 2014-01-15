module Tools.FolToGeo (formulaToSequent, parseFolToSequent)
where

import Data.List
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad.Fix

import qualified Formula.SyntaxFol as Fol
import qualified Formula.SyntaxGeo as Geo
import qualified Utils.FolUtilities as FolU (freeVars, variant, apply)
import qualified Utils.GeoUtilities as GeoU (simplify)


parseFolToSequent :: String -> Maybe [Geo.Sequent]
parseFolToSequent = formulaToSequent.Fol.parseFormula

{-| formulaToSequent is the main function of the module, which may return 
  Just [Sequent] if the input formula is equivalent to a set of geometric 
  sequents, or Nothing otherwise. -}
formulaToSequent :: Fol.Formula -> Maybe [Geo.Sequent]
formulaToSequent fmla = (\(l, r) -> let l's = splitDisjuncts l
                                    in  (\l' -> Geo.Sequent l' r) <$> l's)
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
    where mergeFunc (lp, rp) (lq, rq) = (GeoU.simplify (Geo.And lp lq)
                                        , GeoU.simplify (Geo.Or rp rq))

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
formulaToClause (Fol.Exists x f)    = do
  f' <- formulaToClause f
  case f' of
    Right f'' -> return $ Right $ Geo.Exists x f''
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
convertFormula (Fol.Tru)        = Just Geo.Tru
convertFormula (Fol.Fls)        = Just Geo.Fls
convertFormula (Fol.Atm a)      = Just $ Geo.Atm (convertAtom a)
convertFormula (Fol.Or  p q)    = Geo.Or <$> convertFormula p <*> 
                                  convertFormula q
convertFormula (Fol.And p q)    = Geo.Or <$> convertFormula p <*> 
                                  convertFormula q
convertFormula (Fol.Exists v p) = Geo.Exists v <$> convertFormula p
convertFormula _                = Nothing -- otherwise


convertAtom :: Fol.Atom -> Geo.Atom
convertAtom (Fol.R s ts) = Geo.R s $ convertTerm <$> ts
convertAtom (Fol.F s ts) = Geo.F s $ convertTerm <$> ts

convertTerm :: Fol.Term -> Geo.Term
convertTerm (Fol.Var v)   = Geo.Var v
convertTerm (Fol.Fn s ts) = Geo.Fn s $ convertTerm <$> ts
convertTerm (Fol.Rn s ts) = Geo.Rn s $ convertTerm <$> ts


normalize :: Fol.Formula -> Fol.Formula
normalize = normalize'.pnfAll.elimImp
    where normalize' = fix $ \func x -> let y = (pushExists.pushAnd.nnf) x
                                        in  if x == y then y else func y

-- Implication Elimination
elimImp :: Fol.Formula -> Fol.Formula
elimImp (Fol.Imp p q) = Fol.Or (Fol.Not (elimImp p)) (elimImp q)
elimImp (Fol.Iff p q) = Fol.And (Fol.Or (Fol.Not (elimImp p)) (elimImp q))
                        (Fol.Or (Fol.Not (elimImp q)) (elimImp p))
elimImp (Fol.Not p)   = Fol.Not (elimImp p)
elimImp (Fol.And p q) = Fol.And (elimImp p) (elimImp q)
elimImp (Fol.Or p q)  = Fol.Or  (elimImp p) (elimImp q)
elimImp (Fol.Forall x p) = Fol.Forall x (elimImp p)
elimImp (Fol.Exists x p) = Fol.Exists x (elimImp p)
elimImp f = f  -- otherwise

{- Prenex Universal Quantifiers -}
pnfAll :: Fol.Formula -> Fol.Formula
pnfAll = prenexAll . nnf . simplify
-- 
prenexAll :: Fol.Formula -> Fol.Formula
prenexAll (Fol.Forall x p) = Fol.Forall x (prenexAll p)
-- prenexAll (Fol.Exists x p) = Fol.Exists x (prenexAll p)
prenexAll (Fol.Exists x p) = pullquants (Fol.Exists x (prenexAll p))
prenexAll (Fol.And p q)    = pullquants (Fol.And (prenexAll p) (prenexAll q))
prenexAll (Fol.Or p q)     = pullquants (Fol.Or (prenexAll p) (prenexAll q))
prenexAll fm = fm
-- 
pullquants :: Fol.Formula -> Fol.Formula
pullquants fm = case fm of
 Fol.And (Fol.Forall x p) (Fol.Forall y q) -> 
   pullq (True,True) fm Fol.Forall Fol.And x y p q
 Fol.And (Fol.Forall x p) q -> pullq (True,False) fm Fol.Forall Fol.And x x p q
 Fol.And p (Fol.Forall y q) -> pullq (False,True) fm Fol.Forall Fol.And y y p q
 Fol.Or (Fol.Forall x p) q ->  pullq (True,False) fm Fol.Forall Fol.Or x x p q
 Fol.Or p (Fol.Forall y q) ->  pullq (False,True) fm Fol.Forall Fol.Or y y p q
 Fol.Exists x (Fol.Forall y p) -> pullq' True fm Fol.Forall (Fol.Exists x) y p
 _ -> fm

pullq :: (Bool, Bool) -> Fol.Formula
     -> (Fol.Var -> Fol.Formula -> Fol.Formula)
     -> (Fol.Formula -> Fol.Formula -> Fol.Formula) -> Fol.Var -> Fol.Var
     -> Fol.Formula -> Fol.Formula -> Fol.Formula
pullq (l,r) fm quant op x y p q =
 let z = FolU.variant x (FolU.freeVars fm) 
     p' = if l then FolU.apply (Map.singleton x (Fol.Var z)) p else p
     q' = if r then FolU.apply (Map.singleton y (Fol.Var z)) q else q in
 quant z (pullquants(op p' q'))

pullq' :: Bool -> Fol.Formula
     -> (Fol.Var -> Fol.Formula -> Fol.Formula)
     -> (Fol.Formula -> Fol.Formula) -> Fol.Var
     -> Fol.Formula -> Fol.Formula
pullq' b fm quant op x p =
 let z = FolU.variant x (FolU.freeVars fm) 
     p' = if b then FolU.apply (Map.singleton x (Fol.Var z)) p else p
 in  quant z (pullquants (op p'))
-- 
-- Converts a formula to negation normal form
nnf :: Fol.Formula -> Fol.Formula
nnf (Fol.And p q)          = Fol.And (nnf p) (nnf q)
nnf (Fol.Or p q)           = Fol.Or (nnf p) (nnf q)
nnf (Fol.Imp p q)          = Fol.Or (nnf (Fol.Not p)) (nnf q)
nnf (Fol.Iff p q)          = Fol.Or (Fol.And (nnf p) (nnf q)) 
                            (Fol.And (nnf(Fol.Not p)) (nnf(Fol.Not q)))
nnf (Fol.Not(Fol.Not p))       = nnf p
nnf (Fol.Not(Fol.And p q))     = Fol.Or (nnf(Fol.Not p)) (nnf(Fol.Not q))
nnf (Fol.Not(Fol.Or p q))      = Fol.And (nnf(Fol.Not p)) (nnf(Fol.Not q))
nnf (Fol.Not(Fol.Imp p q))     = Fol.And (nnf p) (nnf(Fol.Not q))
nnf (Fol.Not(Fol.Iff p q))     = Fol.Or (Fol.And (nnf p) (nnf(Fol.Not q))) 
                                 (Fol.And (nnf(Fol.Not p)) (nnf q))
nnf (Fol.Forall x p)       = Fol.Forall x (nnf p)
nnf (Fol.Exists x p)       = Fol.Exists x (nnf p)
nnf (Fol.Not (Fol.Forall x p)) = Fol.Exists x (nnf (Fol.Not p))
nnf (Fol.Not (Fol.Exists x p)) = Fol.Forall x (nnf (Fol.Not p))
nnf fm = fm

pushExists :: Fol.Formula -> Fol.Formula
pushExists (Fol.Not f)      = Fol.Not (pushExists f)
pushExists (Fol.And p q)    = Fol.And (pushExists p) (pushExists q)
pushExists (Fol.Or p q)     = Fol.Or (pushExists p) (pushExists q)
pushExists (Fol.Exists x f) = case pushExists f of -- inside-out processing
                            Fol.Or p q -> Fol.Or (Fol.Exists x p) (Fol.Exists x q)
                            f      -> Fol.Exists x f
pushExists (Fol.Forall x f) = Fol.Forall x (pushExists f)
pushExists f = f -- otherwise

pushAnd :: Fol.Formula -> Fol.Formula
-- pushAnd (Fol.And (Fol.Or p q) r) = Fol.Or (Fol.And p r) (Fol.And q r)
pushAnd (Fol.And r (Fol.Or p q)) = Fol.Or (Fol.And r p) (Fol.And r q)
pushAnd (Fol.Not f)          = Fol.Not (pushAnd f)
pushAnd (Fol.And p q)        = 
    case (pushAnd p, pushAnd q) of -- inside-out processing
      (Fol.Or p' p'', q') -> Fol.Or (Fol.And p' q') (Fol.And p'' q')
      (p', Fol.Or q' q'') -> Fol.Or (Fol.And p' q') (Fol.And p' q'')
      (p', q')        -> Fol.And p' q'
pushAnd (Fol.Or  p q)        = Fol.Or  (pushAnd p) (pushAnd q)
pushAnd (Fol.Forall x p)     = Fol.Forall x (pushAnd p)
pushAnd (Fol.Exists x p)     = Fol.Exists x (pushAnd p)
pushAnd f = f -- otherwise

simplify :: Fol.Formula -> Fol.Formula
simplify (Fol.Not p)      = simplify1 $ Fol.Not $ simplify p
simplify (Fol.And p q)    = simplify1 (Fol.And (simplify p) (simplify q))
simplify (Fol.Or p q)     = simplify1 (Fol.Or (simplify p) (simplify q))
simplify (Fol.Imp p q)    = simplify1 (Fol.Imp (simplify p) (simplify q))
simplify (Fol.Iff p q)    = simplify1 (Fol.Iff (simplify p) (simplify q))
simplify (Fol.Forall x p) = simplify1 $ Fol.Forall x (simplify p)
simplify (Fol.Exists x p) = simplify1 $ Fol.Exists x (simplify p)
simplify fm = fm

simplify1 :: Fol.Formula -> Fol.Formula
simplify1 fm = 
       case fm of
              Fol.Forall x p -> if elem x (FolU.freeVars p) then fm else p
              Fol.Exists x p -> if elem x (FolU.freeVars p) then fm else p
              Fol.Not Fol.Fls   -> Fol.Tru
              Fol.Not Fol.Tru    -> Fol.Fls
              Fol.And Fol.Fls _  -> Fol.Fls
              Fol.And _ Fol.Fls -> Fol.Fls
              Fol.And Fol.Tru q  -> q
              Fol.And p Fol.Tru  -> p
              Fol.Or Fol.Fls q   -> q
              Fol.Or p Fol.Fls  -> p
              Fol.Or Fol.Tru _   -> Fol.Tru
              Fol.Or _ Fol.Tru   -> Fol.Tru
              Fol.Imp Fol.Fls _  -> Fol.Tru
              Fol.Imp Fol.Tru q  -> q
              Fol.Imp _ Fol.Tru  -> Fol.Tru
              Fol.Imp p Fol.Fls -> Fol.Not p
              Fol.Iff Fol.Tru q  -> q
              Fol.Iff p Fol.Tru  -> p
              Fol.Iff Fol.Fls q  -> Fol.Not q
              Fol.Iff p Fol.Fls -> Fol.Not p
              _ -> fm              
