{-|
  Razor
  Module      : Main
  Description : The module provides a REPL for user interaction with Razor.
  Maintainer  : Salman Saghafi, Ryan Danas
-}

module Main where
import API
import Common.Model (Model (..))
import Common.Provenance
import Data.Maybe
import REPL.Syntax
import REPL.Ansi.Display
import Syntax.GeometricUtils (Theory)
import SAT.Impl
import System.Console.Haskeline
import System.Environment
import Tools.Config


import Control.Monad.Trans


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
      prettyPrint fmodel (show model')
      -- enter the repl
      runInputT defaultSettings (loop (model', stream') prov theory)
  -- exit display
  displayExit

loop :: (Maybe Model, SATIteratorType) -> ProvInfo -> Theory -> InputT IO ()
loop (model, stream) prov thy = do
  let sameLoop = loop (model, stream) prov thy
  let newLoop (model', stream') = (lift $ prettyPrint fmodel (show model')) >> loop (model', stream') prov thy
  minput <- getInputLine "% "
  case minput of
      Nothing -> return ()
      Just command -> case (parseCommand command) of
        Nothing -> (lift $ prettyPrint ferror "syntax error\n") >> sameLoop
        Just cmd -> case cmd of
          Go explore -> do
            (model', stream') <- updateState (model, stream) explore 
            case model' of
              Nothing -> (lift $ prettyPrint fwarning "no model found; exploration not applied\n") >> sameLoop
              Just mdl -> newLoop (model', stream)
          Ask question -> case question of
            Name term -> do
              case (getSkolemHead prov term) of
                Nothing -> (lift $ (prettyPrint ferror ("no prov information for " ++ (show term) ++ "\n"))) >> sameLoop
                Just (elm, skolemFn) -> do
                  let subs = (nameTheory elm skolemFn thy)
                  let strs = (map show thy)
                  let replacements = (zip subs strs)
                  mapM (\x -> lift $ (prettyReplace x)) replacements
                  sameLoop
            Blame term -> sameLoop
          Other utility -> case utility of
            Help -> do
              (lift $ prettyPrint finfo helpCommand)
              sameLoop
            Exit -> do
              (lift $ prettyPrint finfo "exiting...\n")
              return ()

updateState :: (Maybe Model, SATIteratorType) -> Explore -> InputT IO (Maybe Model, SATIteratorType)
updateState (model, stream) explore = do
  return $ case explore of
    Next -> do
      nextModel stream
    Augment term -> do
      -- TODO actually augment here
      (model, stream)