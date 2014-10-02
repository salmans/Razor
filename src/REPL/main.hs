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
import REPL.Syntax
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
  -- generate G*, get the model stream, and enter the repl
  let (base, prov, prop) = generateGS config theory
  let stream = modelStream prop 
  runInputT defaultSettings (loop (Nothing, stream))

loop :: (Maybe Model, SATIteratorType) -> InputT IO ()
loop (model, stream) = do
  let continue = loop (model, stream)
  minput <- getInputLine "% "
  case minput of
      Nothing -> return ()
      Just command -> case (parseCommand command) of
        Nothing -> outputStrLn "syntax error" >> continue
        Just cmd -> case cmd of
          Go explore -> do
            (model', stream') <- updateState (model, stream) explore 
            outputStrLn (show model') >> loop (model', stream')
          Ask question -> case question of
            Name term -> continue
            Blame term -> continue
          Other utility -> case utility of
            Help -> do
              outputStrLn helpCommand
              continue
            Exit -> return ()

updateState :: (Maybe Model, SATIteratorType) -> Explore -> InputT IO (Maybe Model, SATIteratorType)
updateState (model, stream) explore = do
  return $ case explore of
    Next -> do
      nextModel stream
    Augment term -> do
      -- TODO actually augment here
      (model, stream)