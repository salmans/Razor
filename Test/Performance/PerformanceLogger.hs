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

  time1 <- getClockTime
  -- get the answer
  let model  = chase' inputFmlas
  time2 <- getClockTime

  let diffTime = tdPicosec $ diffClockTimes time2 time1

  putStrLn $ inputFileName ++ "\t" ++ (show diffTime)