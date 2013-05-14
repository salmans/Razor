{- Time-stamp: <2013-02-11 23:56:27 Salman Saghafi>
  Runs chase' on the input theory and returns the first model
  for the theory (if any exists). It also verifies the output
  model against the input theory.
-}


module Main where
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
      realLines =  filter Utils.Utils.isRealLine inputLines     :: [String]
      inputFmlas =  map parseSequent realLines     :: [Sequent]

  -- get the answer
  let model  = chase' inputFmlas


  -- Verify that every formula in the theory is true:
  let verifyMsg = 
          if Maybe.isJust model
          then (let (Model trs domain) = Maybe.fromJust model 
                    maps f = Utils.Utils.allMaps (fv f) domain
                    fmlas = concatMap insts inputFmlas
                    insts = (\f -> map 
                                   (\s -> onSequent (lift s) f) 
                                   (maps f)) 
                in
--                  (trace (show (filter (\s -> (sequentHolds (Maybe.fromJust model) s)) fmlas)))
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
  putStrLn $ show inputFmlas

  putStrLn "Model: "
  putStrLn $ show model
  --putStrLn ""
  --putStrLn $ "-> " ++ verifyMsg
  putStrLn ""


  --putStrLn  ("Found " ++ (show $ length models) ++  " models ")

-- end main
