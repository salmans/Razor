{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}{-|
  Razor
  Module      : REPL.Main
  Description : The module provides a REPL for user interaction with Razor.
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module Main where
import API.Surface
import REPL.Display
import Common.Model
import Common.Provenance
import Data.Maybe
import Data.List
import Syntax.GeometricUtils 
import SAT.Impl
import System.Console.Haskeline
import Chase.Impl
import System.Environment
import Tools.Config
import Control.Monad.Trans
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Syntax.GeometricParser
import REPL.Mode
import qualified REPL.Mode.Theory as T
import qualified REPL.Mode.Stream as H

data REPLCommand = Display Substate | Change Mode | ModeHelp | Help | Exit
data Substate = TheConfig | TheTheory | TheModel
type Mode = (LoopMode m) => m

--------------------
-- Main REPL/Loop --
--------------------
main :: IO ()
main = do
  -- init display
  displayInit
  -- get configuration
  config <- getConfig 
  -- enter starting mode
  let state = REPLState config Nothing Nothing Nothing Nothing
  let startmode = T.TheoryM
  case enterMode startmode state of
    Left err -> prettyPrint 0 ferror err
    Right state' -> do
      replSplash
      runInputT defaultSettings $ loop state' startmode
  -- exit display
  displayExit

loop :: (LoopMode m) => REPLState -> m -> InputT IO()
loop state@(REPLState config theory gstar stream model) mode = do
  -- loop types
  let stay = loop state mode
  let go state' = loop state' mode
  let chmod state' mode' = loop state' mode'
  -- get input
  lift $ putStr "\n"
  minput <- getInputLine $ modeTag mode
  -- parse input into a command and act depending on the case
  case minput of
      Nothing -> return ()
      Just cmd -> do
        case parseREPLCommand cmd of
          -- Try and run the mode command
          Nothing -> do
            run <- lift $ runOnce mode state cmd
            case run of
              Left err -> lift (prettyPrint 0 ferror err) >> stay
              Right state' -> go state'
          -- Run the overall REPL command
          Just command -> case command of
            Display substate -> case substate of
              TheConfig -> lift (prettyPrint 0 foutput (show config)) >> stay
              TheTheory -> lift (prettyTheory theory) >> stay
              TheModel -> lift (prettyPrint 0 flow (show model)) >> stay
            Change mode' -> case enterMode mode' state of
              Left err -> lift (prettyPrint 0 ferror err) >> stay
              Right state' -> lift (exitMode mode) >> chmod state' mode'
            ModeHelp -> lift (showHelp mode) >> stay
            Help -> lift replHelp >> stay
            Exit -> return ()

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
  "A model finding assistant!"
replHelp :: IO()
replHelp = prettyPrint 0 foutput $ ""++
  "!c             Display The Current Configuration Options\n"++
  "!t             Display The Currently Loaded Theory\n"++
  "!m             Display The Current Model\n"++
  "@t             Enter Theory Editing / Configuration Mode\n"++
  "@v             Enter Modelspace Vertical Exploration Mode\n"++
  "@h             Enter Modelspace Horizontal Exploration Mode\n"++
  "@q             Enter Query Mode\n"++
  "?              Display Mode Specific Help\n"++
  "help           Print This Message\n"++
  "q|quit|exit    Exit Razor"

parseREPLCommand :: String -> Maybe REPLCommand
parseREPLCommand cmd = 
  let pResult = parse pCommand "parsing REPL command" cmd
  in case pResult of
    Left err -> Nothing
    Right val -> Just val

pCommand :: Parser REPLCommand
pCommand = pChange <|> pDisplay <|> pModeHelp <|> pHelp <|> pExit

-- Change
pChange :: Parser REPLCommand
pChange = do
  symbol "@"
  Change <$> pMode

pMode :: Parser Mode
pMode = pTheoryM <|> pStreamM

pTheoryM :: Parser Mode
pTheoryM = symbol "t" >> return T.TheoryM

pStreamM :: Parser Mode
pStreamM = symbol "h" >> return H.StreamM 

-- Display
pDisplay :: Parser REPLCommand
pDisplay = do
  symbol "!"
  Display <$> pSubstate

pSubstate :: Parser Substate
pSubstate = pTheConfig <|> pTheTheory <|> pTheModel

pTheConfig :: Parser Substate
pTheConfig = do
  symbol "c"
  return TheConfig

pTheTheory :: Parser Substate
pTheTheory = symbol "t" >> return TheTheory

pTheModel :: Parser Substate
pTheModel = symbol "m" >> return TheModel

-- Mode Help / Help / Exit
pModeHelp :: Parser REPLCommand
pModeHelp = symbol "?" >> return ModeHelp

pHelp :: Parser REPLCommand
pHelp = symbol "help" >> return Help

pExit :: Parser REPLCommand
pExit = (symbol "q" <|> symbol "quit" <|> symbol "exit") >> return Exit
