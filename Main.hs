{- Time-stamp: <2013-05-14 12:59:51 Salman Saghafi>
  Runs chase' on the input theory and returns the first model
  for the theory (if any exists). It also verifies the output
  model against the input theory.
-}


module Main where
import System.Environment
import System.FilePath
import System.Time
import Debug.Trace
import Control.Exception 
import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as State
import Data.List
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import Chase.Formula.SyntaxGeo
import Chase.Utils.GeoUtilities
import Chase.Tools.FolToGeo
import qualified Chase.Utils.Utils as Utils
import Chase.Tools.GeoUnification
import Chase.Tools.Config
import Chase.Problem.Model
import Chase.Problem.Operations (sequentHolds)
import Chase.Chase
-- ============================
-- Main
-- ============================


main :: IO ()
main = do
  -- get the arguments
  args <- getArgs                                        :: IO [String]
  let inputFileBaseName =  
          if null args then error "need an input file"
          else head args                     :: String
      inputFileName = inputFileBaseName :: FilePath 

      maxIterations = if (length args) > 1 
                      then read (args !! 1) 
                      else -1

  -- do input file reading
  src <- readFile inputFileName                    

  -- get a list of sequents from the input file
  let inputLines = lines src :: [String]
      realLines =  filter Utils.isRealLine inputLines     :: [String]
      inputFmlas = case mapM (parseFolToSequent False) realLines of
                     Nothing    -> error "The input is not geometric!" 
                     Just fmlas -> concat fmlas

  time1 <- getClockTime
  -- get the answer
  let model  = chase' defaultConfig inputFmlas
  time2 <- getClockTime

  let diffTime = diffClockTimes time2 time1

  putStrLn $ "Execution time:" ++ (show diffTime)

  -- Remove functions from the input sequents, like what the chase does:
  let inputFmlas' = renameVars <$> relConvert inputFmlas

  -- verifyAll model inputFmlas'

  -- Verify that every formula in the theory is true:
  let verifyMsg = 
          if Maybe.isJust model
          then (let mdl@(Model trs _) = Maybe.fromJust model 
                    domain = Elm <$> modelDomain mdl
                    maps f = Utils.allMaps (freeVars f) domain
                    insts = (\f -> map 
                                   (\s -> (liftTerm.lift) s f) 
                                   (maps f)) 
                    fmlas = concatMap insts inputFmlas'
                in
                (case all (\s -> (sequentHolds (Maybe.fromJust model) s)) 
                          fmlas of
                True -> "Verified!"
                False -> "Oops! The model does not satisfy the theory." ++
                         (show (filter (\s -> (not(sequentHolds (Maybe.fromJust model) s))) 
                          fmlas))
                ))
          else "Nothing to verify!"


  -- report result
  --
  putStrLn $ "\n" ++ "===================="
  putStrLn $ "file: " ++ show inputFileName
  -- echo the input theory:
  putStrLn "Theory: "
  putStrLn $ show inputFmlas'

  putStrLn "Model: "
  putStrLn $ show model
  --putStrLn ""
  putStrLn $ "-> " ++ verifyMsg
  putStrLn ""


-- QUICK FIX
-- If a variable name is both free and bound in a sequent, rename the bound
-- variables. For the sake of simplicity, we rename all bound variables 
-- (i.e., existential vairables on right)
-- REMARK: This functions assumes normalized sequents:
renameVars :: Sequent -> Sequent
renameVars seq@(Sequent bdy hd) = 
    Sequent bdy (State.evalState (renameHeadVars hd) 0)

-- Salman: all inductions on formula can be done only once.
renameHeadVars :: Formula -> Counter Formula
renameHeadVars (Or f1 f2) = do
  f1' <- renameHeadVars f1
  f2' <- renameHeadVars f2
  return $ Or f1' f2'
renameHeadVars (And f1 f2) = do
  f1' <- renameHeadVars f1
  f2' <- renameHeadVars f2
  return $ And f1' f2'
renameHeadVars (Exists x f) = do
  x' <- freshSymbol "@bv"
  f' <- renameHeadVars f
  let sub = Map.singleton x (Var x')
  return $ Exists x' ((liftTerm.lift) sub f')
renameHeadVars f = return f  --otherwise


-- TO BE USED!
verifyAll mdls inputFmlas = 
    case mdls of
      []    -> print "Nothing to verify!"
      mdls' -> mapM_ (verify inputFmlas) mdls'

-- mapM_ $ verify inputFmlas <$> mdls'

verify inputFmlas mdl@(Model trs _) = 
    traceShow "."
    $
    let domain = Elm <$> modelDomain mdl
        maps f = Utils.allMaps (freeVars f) domain
        fmlas = concatMap insts inputFmlas
        insts = (\f -> map 
                       (\s -> (liftTerm.lift) s f) 
                       (maps f)) 
    in (case all (\s -> (sequentHolds mdl s)) fmlas of
          True  -> print "Verified!"
          False -> print $ 
                   "Oops! The model does not satisfy the theory." ++
                  (show (filter (\s -> (not(sequentHolds mdl s))) 
                         fmlas)))