{- Time-stamp: <2012-12-01 12:05:16 Salman Saghafi>
-}

module Test.AutomatedTest where

import System
import System.FilePath
import Debug.Trace
import Control.Exception 
import Data.List
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import Formula.SyntaxGeo
import Utils.GeoUtilities
import qualified Utils.Utils
import Tools.GeoUnification
import CC.CC as CC
import Chase.Chase
import Chase.Problem.Model
import Chase.Problem.Operations (sequentHolds)
import WeaklyAcyclic.WeaklyAcyclic
import Test.QuickCheck


varNum = 10
constNum = 10
funcNum = 10
relNum = 10
maxRelArity = 3
maxFuncArity = 3
-- fmlaNum = 10 -- How to use?!
-- parameters that control term generation:
varProb = 2 -- 1/varProb is the chance of a term being a variable.
exVarProb = 2 -- 1/exVarProb is the chance of a variable being existential.
constProb = 1 -- 1/constProb is the chance of a function term being a constant.

-- parameters to control formula generation on left:
lTrueProb = 10
lAtomProb = 80
lAndProb = 10

-- parameters to control formula generation on right:
rFalseProb = 10
rAtomProb = 60
rAndProb = 10
rOrProb = 10
rExistsProb = 10
{- ** In this implementation, all functions and relations have arity one. -}

main :: IO() 
main = verboseCheck prop_validModel


-- Implements an Arbitrary instance for Term
instance Arbitrary Term where
    arbitrary = do
      b1 <- choose (1, varProb) :: Gen Int
      b2 <- choose (1, constProb) :: Gen Int
      cInd <- choose (0, constNum - 1) :: Gen Int
      fInd <- choose (0, funcNum - 1) :: Gen Int
      vInd <- choose (0, varNum -1) :: Gen Int
      ts <- leftTerms ((fInd `mod` maxFuncArity) + 1)
      return $ case b1 of
                 1 -> Var ("fv" ++ (show vInd))
                 _ -> case b2 of
                        1 -> Fn ("c" ++ (show cInd)) []
                        _ -> Fn ("f" ++ (show fInd)) ts
    coarbitrary = undefined


-- Uses arbitrary to generate a list of terms of the given 
-- length.
leftTerms :: Int -> Gen [Term]
leftTerms 0 = return []
leftTerms size = do
  ts <- leftTerms (size - 1)
  t <- arbitrary
  return (t:ts)

-- Like arbitrary, returns a generator for creating random terms
-- such that they only contain the variables in the input lists,
-- i.e., existential and universal variables.
rightTerm :: Vars -> Vars -> Gen Term
rightTerm vars exvars = do
      b1 <- choose (1, varProb) :: Gen Int
      b2 <- choose (1, exVarProb) :: Gen Int
      b3 <- choose (1, constProb) :: Gen Int
      cInd <- choose (0, constNum - 1) :: Gen Int
      fInd <- choose (0, funcNum - 1) :: Gen Int
      v <- elements vars :: Gen Var
      v' <- elements exvars :: Gen Var
      ts <- rightTerms ((fInd `mod` maxFuncArity) + 1) vars exvars
      return $ case b1 of
                 x | x == 1 && not (null vars && null exvars)
                       -> case b2 of
                            y | (y == 1 && not (null exvars)) 
                                || (null vars) -> Var v'
                            _ -> Var v
                 _ -> case b3 of
                            1 -> Fn ("c" ++ (show cInd)) []
                            _ -> Fn ("f" ++ (show fInd)) ts

-- Uses rightTerm to generate a list of terms of the given
-- length
rightTerms :: Int -> Vars -> Vars -> Gen [Term]
rightTerms 0 _ _ = return []
rightTerms size vars exvars = do
  ts <- rightTerms (size - 1) vars exvars
  t <- rightTerm vars exvars
  return (t:ts)


-- Generates a random *geometric* formula.
instance Arbitrary Sequent where
    arbitrary = do 
      left <- buildLeft
      right <- buildRight (freeVars left) [] 0
      return $ Sequent left right
    coarbitrary = undefined


-- Builds the left subformulas of a sequent.
buildLeft :: Gen Formula
buildLeft = do
  b <- choose (0, prob3 - 1) :: Gen Int
  rInd <- choose (0, relNum - 1) :: Gen Int
  f1 <- buildLeft
  f2 <- buildLeft
  ts <- leftTerms ((rInd `mod` maxRelArity) + 1)
  return $ case b of
             x | x < prob1 ->
                   Tru
             x | x >= prob1 && x < prob2 ->
                   Atm (R ("R" ++ (show rInd)) ts)
             x | x >= prob2 && x < prob3 ->
                   And f1 f2
  where prob1 = lTrueProb
        prob2 = prob1 + lAtomProb
        prob3 = prob2 + lAndProb

-- Builds the right subformulas of sequents.
buildRight :: Vars -> Vars -> Int -> Gen Formula
buildRight vars exvars ind = do
  b <- choose (0, prob5 - 1) :: Gen Int
  rInd <- choose (0, relNum - 1) :: Gen Int
  f1 <- buildRight vars (("bv" ++ (show ind)):exvars) (ind + 1)
  f2 <- buildRight vars exvars ind
  f3 <- buildRight vars exvars ind
  ts <- rightTerms ((rInd `mod` maxRelArity) + 1) vars exvars
  return $ case b of
             x | x < prob1 ->
                   Fls
             x | x >= prob1 && x < prob2 -> 
                   Atm (R ("R" ++ (show rInd)) ts)
             x | x >= prob2 && x < prob3 -> 
                   Or f2 f3
             x | x >= prob3 && x < prob4 -> 
                   And f2 f3
             x | x >= prob4 && x < prob5 -> 
                  Exists ("bv" ++ (show ind)) f1
  where prob1 = rFalseProb
        prob2 = prob1 + rAtomProb
        prob3 = prob2 + rAndProb
        prob4 = prob3 + rOrProb
        prob5 = prob4 + rExistsProb


-- A theory is either not weakly acyclic or its model makes every
-- formula true.
prop_validModel sequents =
    if weaklyAcyclic sequents
    then case model of
           Nothing -> verify model sequents == Nothing
           otherwise -> verify model sequents == Just True
    else True
    where model = chase' sequents


-- Verify that every formula in the theory is true:
verify :: Maybe Model -> [Sequent] -> Maybe Bool
-- verify Nothing _ = Nothing
-- verify (Just _) _ = Just True
verify model inputFmlas = 
    trace ("verifying")
    (if Maybe.isJust model
    then (let Model trs domain = Maybe.fromJust model 
              maps f = Utils.Utils.allMaps (freeVars f)
                       $ filter (\e -> CC.normalForm trs e /= truth) domain
              fmlas = concatMap insts inputFmlas
              insts = (\f -> map 
                             (\s -> (liftTerm.lift) s f) 
                             (maps f))
          in
          (case all (\s -> (sequentHolds (Maybe.fromJust model) s)) 
                fmlas of               
             True -> Just True
             False -> Just False ))
    else Nothing)