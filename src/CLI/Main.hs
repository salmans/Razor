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
  -- check for the xmlFile location
  case (configState config) of
    Nothing -> error "No XML file path given!"
    Just xmlFile -> do
      -- check for command
      case (configCommand config) of
        -- no command... return start state
        Nothing -> do
          startState <- getStartState config
          case startState of
            Left (UErr err) -> error err
            Right state@(UState theory prov stream model modelProv) -> toXMLFile state Nothing xmlFile
        -- command... read current state + apply command = return state'
        Just command -> do
          state <- fromXMLFile xmlFile
          case applyCommand state command of
            Left (UErr err) -> error err
            Right goodnews -> case goodnews of
              Left answer -> toXMLFile state (Just answer) xmlFile
              Right state' -> toXMLFile state' Nothing xmlFile

applyCommand :: UState -> String -> Either UError (Either UAnswer UState)
applyCommand state command = case (parseCommand command) of
  -- exploration
  Go explore -> case explore of
    Next -> case getNextModel state of
      Left err -> Left err
      Right state' -> Right $ Right state'
    Augment term -> Left (UErr "not implemented")
  -- explanation
  Ask question -> case question of
    Name isall isrec term -> do
      let origin = getOrigin state (isall,isrec) term
      Right $ Left (AOrigin origin)
    Blame atom -> do
      let justification = getJustification state atom
      Right $ Left (ABlame justification)
  -- display
  Display thing -> Left (UErr "CLI does not support display commands!")
  -- others
  Other utility -> Left (UErr "CLI does not support utility commands!")
  -- errors
  SyntaxError err -> Left (UErr ("Command Syntax Error!\n"++err))
