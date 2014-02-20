module Main where
import System
import System.FilePath
import Debug.Trace
import Control.Exception 
import Data.List
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import Chase.Formula.SyntaxFol
import qualified Chase.Utils.Utils as Utils
import Chase.WeaklyAcyclic.WeaklyAcyclic

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
      inputFmlas =  map parseFormula realLines     :: [Formula]      

  -- get the answer
  let isWA  = weaklyAcyclic inputFmlas   


  -- report result
  --
  putStrLn $ "\n" ++ "===================="
  putStrLn $ "file: " ++ show inputFileName
  -- echo the input theory:
  putStrLn "Theory: "
  putStrLn $ show inputFmlas

  putStrLn "Weakly Acyclic: "
  putStrLn $ show isWA
  putStrLn ""
-- end main
