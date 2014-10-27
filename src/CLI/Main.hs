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
import CLI.ToXML
import CLI.FromXML
import Tools.Config

main :: IO ()
main = do
  -- get configuration
  config <- getConfig 
  -- check for the statefile location
  case (configState config) of
    Nothing -> error "No XML state file path given!"
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
          state@(UState theory prov stream model modelProv) <- fromXMLFile stateFile
          case applyCommand state command of
            Left (UErr err) -> error err
            Right state'@(UState theory' prov' stream' model' modelProv') -> toXMLFile (UState theory' prov' stream' model modelProv') stateFile

applyCommand :: UState -> String -> Either UError UState
applyCommand state command = case (parseCommand command) of
  -- exploration
  Go explore -> case explore of
    Next -> getNextModel state
    Augment term -> Left (UErr "not implemented")
  -- explanation
  Ask question -> case question of
    Name isall isrec term -> do
      let origins = getOrigin state (isall,isrec) term
      -- TODO return answer?
      Right state
    Blame atom -> do
      let justification = getJustification state atom
      -- TODO return answer?
      Right state
  -- display
  Display thing -> Left (UErr "CLI does not support display commands!")
  -- others
  Other utility -> Left (UErr "CLI does not support utility commands!")
  -- errors
  SyntaxError err -> Left (UErr ("Command Syntax Error!\n"++err))
