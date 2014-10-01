{-|
  Razor
  Module      : Main
  Description : The module provides a REPL for user interaction with Razor.
  Maintainer  : Salman Saghafi, Ryan Danas
-}

module Main where
import API
import Data.Maybe
import System.Console.Haskeline
import System.Environment
import Tools.Config
 
main :: IO ()
main = do
  -- get configuration
  args <- getArgs
  config <- API.getConfig args
  -- read in user input file
  let inputFileName = if   (isJust.configInput) config
                      then (fromJust.configInput) config
                      else error "No input file is specified!"
  -- parse it into a theory
  input <- readFile inputFileName
  thy <- API.getTheory config input
  theory <- case thy of
    Just thy -> return thy
    Nothing -> error "Unable to parse input theory!"
  -- print preprocess information
  putStrLn $ "\n" ++ "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>"
  putStrLn "Input Options: "
  putStrLn (show config)
  putStrLn "Theory: "
  mapM_ (putStrLn.show) theory
  putStrLn $ "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>" ++ "\n"
  -- enter repl loop
  runInputT defaultSettings (loop config)

loop :: Config -> InputT IO ()
loop config = do
    minput <- getInputLine "% "
    case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do outputStrLn $ "Input was: " ++ input
                         loop config