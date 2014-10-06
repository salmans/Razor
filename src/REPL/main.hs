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
import REPL.Ansi.Display
import SAT.Impl
import System.Console.Haskeline
import System.Environment
import Tools.Config

main :: IO ()
main = do
  -- init display
  displayInit
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
  putStrLn $ "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>"
  putStrLn "Input Options: "
  putStrLn (show config)
  putStrLn "Theory: "
  mapM_ (putStrLn.show) theory
  putStrLn $ "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>"
  -- generate G*, get the model stream
  let (base, prov, prop) = generateGS config theory
  let stream = modelStream prop 
  -- get the first model
  (model', stream') <- return (nextModel stream)
  case model' of
    -- dont repl if no models
    Nothing -> error "No models found!"
    Just mdl -> do
      -- display the first model
      prettyPrint (show model') fmodel
      -- enter the repl
      runInputT defaultSettings (loop (model', stream'))
  -- exit display
  displayExit

loop :: (Maybe Model, SATIteratorType) -> InputT IO ()
loop (model, stream) = do
  let continue = loop (model, stream)
  minput <- getInputLine "% "
  case minput of
      Nothing -> return ()
      Just command -> case (parseCommand command) of
        Nothing -> prettyREPL "syntax error\n" ferror >> continue
        Just cmd -> case cmd of
          Go explore -> do
            (model', stream') <- updateState (model, stream) explore 
            case model' of
              Nothing -> prettyREPL "no next model\n" fwarning >> loop (model, stream)
              Just mdl -> prettyREPL (show model') fmodel >> loop (model', stream')
          Ask question -> case question of
            Name term -> continue
            Blame term -> continue
          Other utility -> case utility of
            Help -> do
              prettyREPL helpCommand finfo
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