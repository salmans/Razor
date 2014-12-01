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

  This module facilitates interacting with TPTP problem and solution 
  libraries.

  Module      : Chase.IChase
  Description : This module implements functions that facilitate working with 
  TPTP problem library.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}
  
module TPTP.ITranslate where

-- Standard
import Data.List
import Data.Maybe
import qualified Data.Map as Map

-- Control

-- import Control.Exception -- for assert
import Control.Monad
import Control.Monad.Identity
import qualified Control.Monad.State as State
import Control.Applicative

-- Common
import Common.Basic (freshSymbol)

-- Syntax
import Syntax.GeometricUtils
import qualified Syntax.FirstOrder as Fol

-- import qualified Tools.GeoUnification as Unif (liftSub)

-- Tools
import qualified Tools.FolToGeo as F2G
import Tools.Counter

-- TPTP
import qualified Codec.TPTP as TP


{-| The function converts a list of TPTP inputs (obtained by parsing a TPTP 
  problem file) to a geometric theory, if it is equivalent to a geometric 
  theory. Otherwise, returns Nothing. It also returns a list of constants in 
  the theory, which can be used for constructing a partial model before running 
  the chase on TPTP CNFs. -}
translate :: [TP.TPTP_Input] -> Maybe (Theory, [Constant])
translate inps = do
  (thy, ts, c) <- 
      foldM (\(fs, ts, c) i -> do 
               let cnjr = (TP.unrole.TP.role) i == "conjecture"
               let (f, t) = inputToFol i
               f' <- F2G.formulaToSequents cnjr f
               let (f'', t', c'') = 
                       if cnjr
                       then let (cjrF, cjrT, cjrC) = conjecturize 0 f'
                            in  (cjrF, t ++ cjrT, cjrC)
                       else (f', t, c)
               return  (fs ++ f'', ts `union` t', c'')) ([], [], 0) inps'
  return (thy, ts)
    where inps' = filter filterFunc inps
          filterFunc (TP.AFormula _ _ _ _) = True
          filterFunc (TP.Comment _)        = False


conjecturize c seqs = 
    let (result, c') = State.runState (mapM conjecture seqs) c
        (seqs', ts') = unzip result
    in  (concat seqs', nub $ concat ts', c')

{- Converts a TPTP input to a FOL formula, also returns the constants in the
   TPTP formula. The constants will be used to construct a partial model on 
   which the chase is executed when the input is a CNF.-}
inputToFol :: TP.TPTP_Input -> (Fol.Formula, [Constant])
inputToFol = formulaToFol.TP.formula

{- Converts a TPTP formula to a FOL formula, also returns the constants in the
   TPTP formula. The constants will be used to construct a partial model on 
   which the chase is executed when the input is a CNF.-}
formulaToFol :: TP.Formula -> (Fol.Formula, [Constant])
formulaToFol (TP.F f) =
    case runIdentity f of
      (TP.:~:) f'              -> let (f'', ts) = formulaToFol f'
                                  in  (Fol.Not f'', ts)
      TP.Quant TP.All vs f'    -> 
          let vs'        = (\(TP.V v) -> v) <$> vs
              (base, ts) = formulaToFol f'
          in  (foldr (\v f -> Fol.Forall (Variable v) f) base vs', ts)
      TP.Quant TP.Exists vs f' -> 
          let vs'        = (\(TP.V v) -> v) <$> vs
              (base, ts) = formulaToFol f'
          in  (foldr (\v f -> Fol.Exists Nothing (Variable v) f) base vs', ts)
      TP.BinOp p (TP.:|:) q   -> let (p', pts) = formulaToFol p
                                     (q', qts) = formulaToFol q                                                 
                                 in  (Fol.Or p' q', pts `union` qts)
      TP.BinOp p (TP.:&:) q   -> let (p', pts) = formulaToFol p
                                     (q', qts) = formulaToFol q                                                 
                                 in  (Fol.And p' q', pts `union` qts)
      TP.BinOp p (TP.:=>:) q  -> let (p', pts) = formulaToFol p
                                     (q', qts) = formulaToFol q                                                 
                                 in  (Fol.Imp p' q', pts `union` qts)
      TP.BinOp p (TP.:<=>:) q -> let (p', pts) = formulaToFol p
                                     (q', qts) = formulaToFol q                                                 
                                 in  (Fol.Iff p' q', pts `union` qts)
      TP.PredApp (TP.AtomicWord "$true")  [] -> (Fol.Tru, [])
      TP.PredApp (TP.AtomicWord "$false") [] -> (Fol.Fls, [])
      TP.PredApp (TP.AtomicWord s) ts -> 
                       let (ts', cs) = unzip $ map termToFol ts
                           cs'       = foldr union [] cs
                       in  (Fol.Atm (Fol.Rel s ts'), cs')
      TP.InfixPred t1 (TP.:=:) t2 -> 
                       let (t1', cs1) = termToFol t1
                           (t2', cs2) = termToFol t2
                       in  (Fol.Atm (Fol.Rel "=" [t1', t2']), cs1 `union` cs2)
      TP.InfixPred t1 (TP.:!=:) t2 -> 
                       let (t1', cs1) = termToFol t1
                           (t2', cs2) = termToFol t2
                       in  ( Fol.Not $ Fol.Atm (Fol.Rel "=" [t1', t2'])
                           , cs1 `union` cs2)


termToFol :: TP.Term -> (Term, [Constant])
termToFol trm@(TP.T t) = 
    case runIdentity t of
      TP.Var (TP.V v) -> (Var (Variable v), [])
      TP.FunApp aw@(TP.AtomicWord f) [] ->
          (Cons (Constant f), [Constant f])
      TP.FunApp aw@(TP.AtomicWord f) ts -> 
          let (ts', cs) = unzip $ map termToFol ts
              cs'       = foldr union [] cs
          in  (Fn f ts', cs')
      TP.NumberLitTerm d -> (NumberTerm d, [])
      TP.DistinctObjectTerm s -> (DistinctTerm s, [])

{-| Converts a set of sequent to a set of conjecture sequents. It uses a 
  counter to create indices for constants. The function also returns a list of 
  terms created during the process. -}
conjecture :: Sequent -> Counter ([Sequent], [Constant])
conjecture seq = do
  let vs = freeVars seq
  cs <- mapM (\v -> do { c <- freshSymbol "cjr"; return (Constant c)}) vs
  let sub  = Map.fromList $ zip vs (Cons <$> cs)
  let (Sequent bdy hd) = substitute sub seq -- liftTerm (Unif.liftSub sub) seq
  let hdSequents = (\s -> Sequent s Fls) <$> (splitHead hd)
  let bdySequent = Sequent Tru bdy
  let filtered   = 
          filter (\s -> case s of
                          Sequent Tru Tru -> False
                          Sequent Fls Fls -> False
                          otherwise       -> True) (bdySequent:hdSequents)
  return $  (filtered, cs)
    where splitHead (Or p q)       = splitHead p ++ splitHead q
          splitHead (Exists _ x p) = [p]  -- Assuming normalized sequents
          splitHead p              = [p]