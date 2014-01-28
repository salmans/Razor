{- 
   Given an input TPTP problem file, prints the file name if the theory is 
   geometric and weakly acyclic.
-}


module Main where
import System.Environment
import System.FilePath

import Data.Maybe
import Data.List

import TPTP.TPTPToGeo as T2G
import Codec.TPTP as TP

import System.FilePath.Posix
import System.Directory

import Debug.Trace

import Chase.Chase
import WeaklyAcyclic.WeaklyAcyclic
import Tools.FolToGeo
import qualified Utils.Utils
-- ============================
-- Main
-- ============================

-- The path to the TPTP library is hard-coded for now:
-- tptpPath = "/Programming/TPTP-v6.0.0/"
tptpPath = "./"


-- geoPath = "/Users/Salman/Desktop/GeoLog/"

-- main :: IO ()
-- main = do
--   args <- getArgs :: IO [String]
--   let inputFileBaseName =  
--           if null args then error "need an input file"
--           else head args :: String
--       inputFileName = inputFileBaseName :: FilePath 

--       maxIterations = if (length args) > 1 
--                       then read (args !! 1) 
--                       else -1

--   src <- readFile inputFileName
--   let inputLines = (lines src) -- intersperse "\n" (lines src)

--   mapM_ (\f -> let (_, fn) = splitFileName f
--                in  copyFile f (geoPath ++ fn)) inputLines

--   return ()


main :: IO ()
main = do
  -- get the arguments
  -- args <- getArgs :: IO [String]
  -- let inputFileBaseName =  
  --         if null args then error "need an input file"
  --         else head args :: String
  --     inputFileName = inputFileBaseName :: FilePath 

  --     maxIterations = if (length args) > 1 
  --                     then read (args !! 1) 
  --                     else -1

  -- tptpInputs <- recursiveLoad inputFileName

  --if (weaklyAcyclic.fst.fromJust) $ T2G.inputsToGeo (concat tptpInputs)

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

  let inputLines = lines src :: [String]
      realLines =  filter Utils.Utils.isRealLine inputLines     :: [String]
      inputFmlas = case mapM (parseFolToSequent False) realLines of
                     Nothing    -> error "The input is not geometric!" 
                     Just fmlas -> concat fmlas

  if weaklyAcyclic inputFmlas
  then putStrLn inputFileName
  else return ()


-- Recursively loads the contents of a file and the files included in it, and
-- returns the content of each file as a separate list of inputs.
recursiveLoad :: String -> IO [[TP.TPTP_Input]]
recursiveLoad fName = do 
  { src <- readFile fName
  ; let inputLines = intersperse "\n" (lines src)
  ; let tptpInputs = (TP.parse.concat) inputLines
  ; let (incls, fmlas) = partition isInclude tptpInputs
  ; rest <- mapM (recursiveLoad.includePath) incls
  ; return (fmlas:concat rest) }

isInclude :: TP.TPTP_Input -> Bool
isInclude (TP.Include _ _) = True
isInclude _                = False

includePath :: TP.TPTP_Input -> String
includePath (TP.Include p _) = tptpPath ++ p
includePath _                = ""