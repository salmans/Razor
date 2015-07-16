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

  Module      : REPL.Main
  Description : The module provides a REPL for user interaction with Razor.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}
module Main where
import API.Surface
import REPL.Display
import Common.Model
import Common.Provenance
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Syntax.GeometricUtils
import Syntax.IGeometric
import SAT.Impl
import System.Console.Haskeline
import Chase.Impl
import System.Environment
import Tools.Config
import Control.Monad.Trans
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Text.Parsec.Token
import REPL.Mode
import qualified REPL.Mode.Theory as T
import qualified REPL.Mode.ModelCheck as I
import qualified REPL.Mode.Explore as M
import qualified REPL.Mode.Explain as Q

data REPLCommand = Display Substate | ModeHelp | Help | Exit
data Substate = TheConfig | TheTheory | TheModel

--------------------
-- Main REPL/Loop --
--------------------
main :: IO ()
main = do
  -- init display
  displayInit
  -- get starting state
  state <- setupState 
  -- enter starting mode
  let startmode = T.TheoryM
  enter <- enterMode startmode state
  case enter of
    Left err -> prettyPrint 0 ferror err
    Right stout -> do
      replSplash
      let (state', stin') = update startmode stout state
      endstate <- runInputT defaultSettings $ loop state' startmode stin'
      return $ teardownState endstate
  -- exit display
  displayExit

loop :: (LoopMode m i o) => RazorState -> m -> i -> InputT IO(RazorState)
loop state@(RazorState config theory mspace mcoor) mode stin = do
  -- get input
  minput <- getInputLine $ "Razor/"++(modeTag mode)++"> "
  -- parse input into a command and act depending on the case
  case minput of
      Nothing -> finish
      Just cmd -> do
        case parseREPLCommand cmd of
          -- Run the overall REPL command
          Just command -> case command of
            Display substate -> case substate of
              TheConfig -> lift (prettyPrint 0 foutput ((show config)++"\n")) >> stay
              TheTheory -> lift (prettyTheory theory) >> stay
              TheModel -> lift (prettyModel (modelLookup mspace mcoor)) >> stay
            ModeHelp -> do
              lift $ showHelp T.TheoryM
              lift $ showHelp M.ExploreM
              lift $ showHelp Q.ExplainM
              stay
            Help -> lift replHelp >> stay
            Exit -> finish
          -- Auto execute command for the proper mode
          Nothing -> do
            case (check T.TheoryM cmd, check M.ExploreM cmd, check Q.ExplainM cmd) of
              (True, _, _) -> trans cmd T.TheoryM state
              (_, True, _) -> eTrans cmd I.ModelCheckM M.ExploreM
              (_, _, True) -> eTrans cmd I.ModelCheckM Q.ExplainM
              _ -> lift (prettyPrint 0 ferror "Command not found!\n") >> stay
  where
    -- execute command in given mode with the given state
    exec cmd mode stin = do
      run <- lift $ runOnce mode stin cmd
      case run of
        Left err -> lift (prettyPrint 0 ferror (err++"\n")) >> stay
        Right stout -> do
          let (state', stin') = update mode stout state
          loop state' mode stin'
    -- stay in the same mode / state; no changes
    stay = loop state mode stin
    -- finish the REPL
    finish = return (state)
    -- change the REPL mode; update state as well
    trans cmd m' s = do
      enter <- lift $ enterMode m' s
      case enter of
        Left err -> lift (prettyPrint 0 ferror err) >> stay
        Right stout -> do
          let (s', stin') = update m' stout s
          exec cmd m' stin'
    -- enter an implicit mode and get the updated state without looping
    eState m' = do
      enter <- lift $ enterMode m' state
      case enter of
        Left err -> do
          lift (prettyPrint 0 ferror (err++"\n"))
          return Nothing
        Right stout -> do
          let (state', _) = (update m' stout state)
          return $ Just state'
    -- transition from an implicit mode state to another state
    eTrans cmd m' m'' = do
      s' <- eState m'
      case s' of
        Nothing -> stay
        Just state' -> trans cmd m'' state'

------------------------
-- Main REPL Commands --
------------------------
replSplash :: IO()
replSplash = prettyPrint 0 foutput $ ""++
  "Welcome to\n"++
  "   / __ \\____ _____  ____  _____\n"++
  "  / /_/ / __ `/_  / / __ \\/ ___/\n"++
  " / _, _/ /_/ / / /_/ /_/ / /    \n"++
  "/_/ |_|\\__,_/ /___/\\____/_/\n"++
  "A model finding assistant!\n"++
  "\n"++
  "Distributed under GNU GPL v3 or later\n"++
  "See LICENSE or <http://www.gnu.org/licenses/> for more details\n"
replHelp :: IO()
replHelp = prettyPrint 0 foutput $ ""++
  "!c           Display Configuration Settings\n"++
  "!t           Display Loaded Theory\n"++
  "!m           Display Current Model\n"++
  "help         Print This Message\n"++
  "?            Show Help For Every Command\n"++
  "q|quit|exit  Exit Razor\n"
parseREPLCommand :: String -> Maybe REPLCommand
parseREPLCommand cmd = 
  let pResult = parse pCommand "parsing REPL command" cmd
  in case pResult of
    Left err -> Nothing
    Right val -> Just val

pCommand :: Parser REPLCommand
pCommand = pDisplay <|> pModeHelp <|> pHelp <|> pExit

-- Display
pDisplay :: Parser REPLCommand
pDisplay = do
  string "!"
  Display <$> pSubstate

pSubstate :: Parser Substate
pSubstate = pTheConfig <|> pTheTheory <|> pTheModel

pTheConfig :: Parser Substate
pTheConfig = string "c" >> return TheConfig

pTheTheory :: Parser Substate
pTheTheory = string "t" >> return TheTheory

pTheModel :: Parser Substate
pTheModel = string "m" >> return TheModel

-- REPLMode Help / Help / Exit
pModeHelp :: Parser REPLCommand
pModeHelp = string "?" >> return ModeHelp

pHelp :: Parser REPLCommand
pHelp = string "help" >> return Help

pExit :: Parser REPLCommand
pExit = (string "q" <|> string "quit" <|> string "exit") >> return Exit
