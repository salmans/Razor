{-|
  Razor
  Module      : CLI.Main
  Description : The module provides a Command Line Interface for interation and integration with Razor.
  Maintainer  : Salman Saghafi, Ryan Danas
-}
{-| TODO / BUGS
change errors and output to XML
-}

module Main where
import API.Surface
import API.UserSyntax
import Tools.Config

main :: IO ()
main = do
  -- get configuration
  config <- getConfig 
  -- check for command
  case (configCommand config) of
    -- no command = return start state as XML
    Nothing -> do
      startState <- getStartState config
      case startState of
        Left (UErr err) -> error err
        Right state@(theory, prov, stream, model, modelProv) -> do
          putStrLn $ "THEORY = "++(show theory)
          putStrLn $ "PROV = "++(show prov)
          putStrLn $ "MODEL = "++(show model)
    -- command = parse command, get state, and return the action results as XML
    Just command -> do
      putStrLn $ "COMMAND = "++(show command)
      case (configState config) of
        Nothing -> error "CLI command given with no CLI XML state!"
        Just stateFile -> do
          putStrLn $ "XMLFILE = "++(show stateFile)

