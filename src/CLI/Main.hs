{- This file is part of Razor.

  Razor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Razor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Razor.  If not, see <http://www.gnu.org/licenses/>.

  Module      : CLI.Main
  Description : The module provides a Command Line Interface for interation and 
  integration with Razor.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
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
            Right state@(UState (cfg, thy) (b,p,t) (stream, mdl) modelProv) -> toXMLFile state Nothing xmlFile
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
    Next -> Left (UErr "not implemented")
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
