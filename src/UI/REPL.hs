{-|
  Razor
  Module      : Main
  Description : The module provides a REPL for user interaction with Razor.
  Maintainer  : Salman Saghafi, Ryan Danas
-}

module Main where
import API
import Common.Model (Model (..))
import Data.Maybe
import SAT.Impl
import System.Console.Haskeline
import System.Environment
import Tools.Config
 
main :: IO ()
main = do
  -- get configuration
  args <- getArgs
  config <- API.parseConfig args
  -- read in user input file
  let inputFileName = if   (isJust.configInput) config
                      then (fromJust.configInput) config
                      else error "No input file is specified!"
  input <- readFile inputFileName
  -- parse it into a theory
  thy <- API.parseTheory config input
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
  -- generate G* and the model stream and the first model from the parsed theory
  (base, prov, prop) <- generateGS config theory
  stream <- modelStream prop 
  (first, stream') <- nextModel stream
  -- get the first model and enter repl loop
  runInputT defaultSettings (loop first stream')

loop :: Maybe Model -> SATIteratorType -> InputT IO ()
loop model stream = do
    -- display current model
    outputStrLn $ show model
    -- get user input
    minput <- getInputLine "% "
    case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do outputStrLn $ "Input was: " ++ input
                         loop model stream