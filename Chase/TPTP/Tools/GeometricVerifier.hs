{- 
   Given an input TPTP problem file, returns the name of the file if it is
   a geometric theory.
-}


module Main where
import System.Environment
import System.FilePath

import Data.Maybe
import Data.List

import Chase.TPTP.TPTPToGeo as T2G
import Codec.TPTP as TP

import System.FilePath.Posix
import System.Directory

import Debug.Trace

import Chase.Chase
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
  args <- getArgs :: IO [String]
  let inputFileBaseName =  
          if null args then error "need an input file"
          else head args :: String
      inputFileName = inputFileBaseName :: FilePath 

      maxIterations = if (length args) > 1 
                      then read (args !! 1) 
                      else -1

  tptpInputs <- recursiveLoad inputFileName

  if isJust $ T2G.inputsToGeo (concat tptpInputs)
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