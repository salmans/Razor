{- Time-stamp: <2012-12-01 12:05:16 Salman Saghafi>
-}

--module Test.AutomatedTest where
module Main where

import System.Environment
import System.IO
import System.FilePath
import Debug.Trace
import Control.Applicative
import Control.Monad
import Control.DeepSeq
import Data.List
import Data.Maybe
import qualified Data.Either as Either
import qualified Data.Map as Map

import Formula.SyntaxGeo
import Utils.GeoUtilities
import qualified Utils.Utils as Utils
import Tools.GeoUnification
import qualified RelAlg.DB as DB

import Chase.Chase
import Chase.Problem.Model
import Chase.Problem.BaseTypes
import Chase.Problem.Operations (sequentHolds)
import Chase.Problem.RelAlg.RelAlg
import WeaklyAcyclic.WeaklyAcyclic
import Test.QuickCheck

import Tools.Config

import FCAtlas

varNum = 10
constNum = 10
funcNum = 10
relNum = 10
maxRelArity = 3
maxFuncArity = 3
-- fmlaNum = 10 -- How to use?!
-- parameters that control term generation:
varProb = 1 -- 1/varProb is the chance of a term being a variable.
exVarProb = 4 -- 1/exVarProb is the chance of a variable being existential.
constProb = 1 -- 1/constProb is the chance of a function term being a constant.

-- parameters to control formula generation on left:
lTrueProb = 10
lAtomProb = 80
lAndProb = 10

-- parameters to control formula generation on right:
rFalseProb = 0
rAtomProb = 45
rAndProb = 30
rOrProb = 10
rExistsProb = 15

-- For randomized theories
minSequents = 2
maxSequents = 10
-- For initial models
minFacts = 2
maxFacts = 30
minElms = 2
maxElms = 40
-- Relation signature, for both sequents and facts
relSig = [("P", 1), ("Q", 2), ("R", 2), ("S", 2), ("T", 3), ("U", 3)]

-- main :: IO() 
-- main = verboseCheck prop_validModel


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

-- temporarily replaces Theory

genThy :: Gen Theory
genThy = do
    numSeqs <- choose (minSequents, maxSequents) :: Gen Int
    seqs <- vector numSeqs :: Gen [Sequent]
    return seqs




-- Builds the left subformulas of a sequent.
buildLeft :: Gen Formula
buildLeft = do
  b <- choose (0, prob3 - 1) :: Gen Int
  (rel,arity) <- elements relSig
  f1 <- buildLeft
  f2 <- buildLeft
  ts <- leftTerms arity
--  ts <- leftTerms ((rInd `mod` maxRelArity) + 1)
  return $ case b of
             x | x < prob1 ->
                   Tru
             x | x >= prob1 && x < prob2 ->
                   Atm (R rel ts)
             x | x >= prob2 && x < prob3 ->
                   And f1 f2
  where prob1 = lTrueProb
        prob2 = prob1 + lAtomProb
        prob3 = prob2 + lAndProb

-- Builds the right subformulas of sequents.
buildRight :: Vars -> Vars -> Int -> Gen Formula
buildRight vars exvars ind = do
  b <- choose (0, prob5 - 1) :: Gen Int
  (rel,arity) <- elements relSig
  f1 <- buildRight vars (("bv" ++ (show ind)):exvars) (ind + 1)
  f1' <- buildRight' vars (("bv" ++ (show ind)):exvars) (ind + 1)
  f2 <- buildRight vars exvars ind
  f2' <- buildRight' vars exvars ind
  f3 <- buildRight vars exvars ind
  f3' <- buildRight' vars exvars ind
  ts <- rightTerms arity vars exvars
  return $ case b of
             x | x < prob1 ->
                   Fls
             x | x >= prob1 && x < prob2 -> 
                   Atm (R rel ts)
             x | x >= prob2 && x < prob3 -> 
                   Or f2 f3
             x | x >= prob3 && x < prob4 -> 
                   And f2' f3'
             x | x >= prob4 && x < prob5 -> 
                  Exists ("bv" ++ (show ind)) f1'
  where prob1 = rFalseProb
        prob2 = prob1 + rAtomProb
        prob3 = prob2 + rOrProb
        prob4 = prob3 + rAndProb
        prob5 = prob4 + rExistsProb


buildRight' :: Vars -> Vars -> Int -> Gen Formula
buildRight' vars exvars ind = do
  b <- choose (0, prob5 - 1) :: Gen Int
  (rel,arity) <- elements relSig
  f1 <- buildRight' vars (("bv" ++ (show ind)):exvars) (ind + 1)
  f2 <- buildRight' vars exvars ind
  f3 <- buildRight' vars exvars ind
  ts <- rightTerms arity vars exvars
  return $ case b of
             x | x < prob1 ->
                   Fls
             x | x >= prob1 && x < prob3 -> 
                   Atm (R rel ts)
             x | x >= prob3 && x < prob4 -> 
                   And f2 f3
             x | x >= prob4 && x < prob5 -> 
                  Exists ("bv" ++ (show ind)) f1
  where prob1 = rFalseProb
        prob2 = prob1 + rAtomProb
        prob3 = prob2 + rOrProb
        prob4 = prob3 + rAndProb
        prob5 = prob4 + rExistsProb


-- A theory is either not weakly acyclic or its model makes every
-- formula true.
prop_validModel sequents =
  if   not (weaklyAcyclic sequents)
  then True
  else let models = chase defaultConfig sequents in
       traceShow sequents (traceShow models True)

arg = Args {replay = Nothing,
  maxSuccess = 20,
  maxDiscardRatio = 10,
  maxSize = 20,
  chatty = True}


q = quickCheckWith arg prop_validModel


r :: IO()
r = do
  thys <- sample' genThy
  let thy = head thys

  --putStrLn $ "  Thys : " ++ (show $ length thys)
  --putStrLn $ "  Thy  : " ++ (show $ length thy)
  --putStrLn $ show thy

  if weaklyAcyclic thy
  then r1 thy
  else return ()

  
r1 :: Theory -> IO()
r1 thy = do
--  putStrLn $ show thy

  initModels <- sample' genInitModel
  let initModel = head initModels
--  putStrLn $ show initModel

  let models = chaseWithModel defaultConfig thy initModel
--  putStrLn ""
--  putStrLn $ show $ head models

  if null models
  then return ()
  else do
    { putStrLn "*******************************"
    ; --putStrLn $ show thy
    ; --putStrLn $ "\n\n" ++ (show $ head models) ++ "\n"
    ; runCoreTests models 5 True True -- first n models, use sig test
    ; hFlush stdout
    ; runCoreTests models 5 False True -- first n models, don't use sig test
    ; hFlush stdout
    }

rloop :: Int -> IO()
rloop num = do
  if num <= 0
  then return ()
  else do
  {
  ; r
  ; rloop (num - 1)
  }
  
main :: IO ()
main = do
  rloop 5



genFact :: Int -> Gen Obs
genFact numElms = do
  (rel,arity) <- elements relSig
  args <- genArgs arity
  return $ Fct $ R rel args

  where genArg = do
          a <- choose (1, numElms) :: Gen Int
          return $ Elm $ Elem $ (++) "a" (show a)
        genArgs 0 = return []
        genArgs n = do
          arg <- genArg
          args <- genArgs (n-1)
          return (arg:args)


genInitModel :: Gen Model
genInitModel = do
  numFacts <- choose (minFacts, maxFacts) :: Gen Int
  numElms <- choose (minElms, maxElms) :: Gen Int
  facts <- genFacts numFacts numElms
  return $ modelFromFacts facts

  where genFacts 0 _ = return []
        genFacts n ne = do
          f <- genFact ne
          fs <- genFacts (n-1) ne
          return (f:fs)



modelFromFacts :: [Obs] -> Model
modelFromFacts facts =
  let domTbl = Map.singleton DomTable es
      tbls = foldl addFact domTbl facts
      sks  = (\(Elem s) -> Fn s []) <$> ts
      hist = zip ts sks
  in  emptyModel { modelTables = tbls, modelProvInfo = provs, modelElemHist = hist }
  where es = DB.Set $ [ts]
        ts = nub.concat $ (\(Fct (R _ tl)) -> (\(Elm e) -> e) <$> tl) <$> facts
        provs = ProvInfo (Map.fromList $ (\f -> (f, initProv)) <$> facts) 0
        initProv = [ChaseProv (-1) (-1) Map.empty]
--        initProv = UserProv
        addFact tbls f = let (Fct (R relName args)) = f
                             relTbl = RelTable relName
                             record = (\(Elm e) -> e) <$> args
                             recSet = DB.Set [record]
                             combineRecord _ (DB.Set b) = (DB.Set (record:b))
                         in Map.insertWith combineRecord relTbl recSet tbls

facts1 = [Fct (R "P" [Elm (Elem "a"), Elm (Elem "b")])
         ,Fct (R "Q" [Elm (Elem "d")])
         ,Fct (R "P" [Elm (Elem "b"), Elm (Elem "c")])
         ,Fct (R "Q" [Elm (Elem "a")])]

facts2 = [Fct (R "P" [Elm (Elem "a12")]) , Fct (R "P" [Elm (Elem "a6")]) , Fct (R "Q" [Elm (Elem "a7"), Elm (Elem "a2")]) , Fct (R "Q" [Elm (Elem "a12"), Elm (Elem "a5")]) , Fct (R "Q" [Elm (Elem "a6"), Elm (Elem "a8")]) , Fct (R "Q" [Elm (Elem "a10"), Elm (Elem "a6")]) , Fct (R "R" [Elm (Elem "a13"), Elm (Elem "a3"), Elm (Elem "a9")]) , Fct (R "R" [Elm (Elem "a4"), Elm (Elem "a14"), Elm (Elem "a4")]) , Fct (R "R" [Elm (Elem "a5"), Elm (Elem "a5"), Elm (Elem "a5")]) , Fct (R "R" [Elm (Elem "a2"), Elm (Elem "a2"), Elm (Elem "a2")]) , Fct (R "R" [Elm (Elem "a8"), Elm (Elem "a8"), Elm (Elem "a8")]) , Fct (R "S" [Elm (Elem "a2"), Elm (Elem "a13")]) , Fct (R "S" [Elm (Elem "a5"), Elm (Elem "a5")]) , Fct (R "S" [Elm (Elem "a8"), Elm (Elem "a11")]) , Fct (R "S" [Elm (Elem "a2"), Elm (Elem "a2")]) , Fct (R "S" [Elm (Elem "a8"), Elm (Elem "a8")])]


model1 = modelFromFacts facts1
model2 = modelFromFacts facts2 -- This is used to fix a bug in FindCore (incorrect rigid domain)

gt = genThy

st = do
  l <- sample' gt
  return $ head l

