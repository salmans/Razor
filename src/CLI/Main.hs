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
import CLI.XML
import Tools.Config

main :: IO ()
main = do
  -- get configuration
  config <- getConfig 
  -- check for the statefile location
  case (configState config) of
    Nothing -> error "No CLI XML state file location given!"
    Just stateFile -> do
      -- check for command
      case (configCommand config) of
        -- no command... return start state
        Nothing -> do
          startState <- getStartState config
          case startState of
            Left (UErr err) -> error err
            Right state@(UState theory prov stream model modelProv) -> toXMLFile state stateFile
        -- command... read current state + apply command = return state'
        Just command -> do
          putStrLn $ "no supported commands yet!"
