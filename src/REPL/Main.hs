{-# LANGUAGE ScopedTypeVariables #-}
{-|
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
--import qualified REPL.Mode.Stream as H

data REPLCommand = Display Substate | Change REPLMode | ModeHelp | Help | Exit
data Substate = TheConfig | TheTheory | TheModel
data REPLMode = ModeTheory | ModeStream

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
    Right (mode', modein', modeout') -> do
      replSplash
      let state' = update mode' modeout' state
      endstate <- runInputT defaultSettings $ loop state' mode' modein'
      return $ teardownState endstate
  -- exit display
  displayExit

loop :: (LoopMode m i o) => RazorState -> m -> i -> InputT IO(RazorState)
loop state@(RazorState config theory _ _ model) mode modein = do
  -- get input
  lift $ putStr "\n"
  minput <- getInputLine $ modeTag mode
  -- parse input into a command and act depending on the case
  case minput of
      Nothing -> finish
      Just cmd -> do
        case parseREPLCommand cmd of
          -- Try and run the mode command
          Nothing -> do
            run <- lift $ runOnce mode modein cmd
            case run of
              Left err -> lift (prettyPrint 0 ferror err) >> stay
              Right modeout -> go modeout
          -- Run the overall REPL command
          Just command -> case command of
            Display substate -> case substate of
              TheConfig -> lift (prettyPrint 0 foutput (show config)) >> stay
              TheTheory -> lift (prettyTheory theory) >> stay
              TheModel -> lift (prettyModel model) >> stay
            Change m -> case m of
              ModeTheory -> change T.TheoryM
              ModeStream -> stay--change H.StreamM
            ModeHelp -> lift (showHelp mode) >> stay
            Help -> lift replHelp >> stay
            Exit -> finish
  where
    -- stay in the same mode / state; no changes
    stay = loop state mode modein
    -- update the state with the changes the mode made
    go modeout = do
      let state' = update mode modeout state
      loop state' mode modein
    -- finish the REPL
    finish = return (state)
    -- change the REPL mode; update state as well
    change m' = do
      enter <- lift $ enterMode m' state
      case enter of
        Left err -> lift (prettyPrint 0 ferror err) >> stay
        Right (mode', modein', modeout') -> do
          let state' = update mode' modeout' state
          loop state' mode' modein'

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
  "<expr>:= |   !<substate>       Display information\n"++
  "   <substate>:=  |   c           Configuration\n"++
  "                 |   t           Loaded Theory\n"++
  "                 |   m           Current Model\n"++
  "         |   @<mode>           Transition to a different REPL Mode\n"++
  "   <mode>:=    |   t             Edit Theory and Configuration\n"++
  "               |   v             Vertically Explore Modelspace\n"++
  "               |   h             Horizontally Explore Modelspace\n"++
  "               |   q             Query Current Model\n"++
  "         |   ?                 Show REPLMode Specific Help\n"++
  "         |   help              Print This Message\n"++
  "         |   q|quit|exit       Exit Razor"
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

pMode :: Parser REPLMode
pMode = pTheoryM <|> pStreamM

pTheoryM :: Parser REPLMode
pTheoryM = symbol "t" >> return ModeTheory

pStreamM :: Parser REPLMode
pStreamM = symbol "h" >> return ModeStream

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

-- REPLMode Help / Help / Exit
pModeHelp :: Parser REPLCommand
pModeHelp = symbol "?" >> return ModeHelp

pHelp :: Parser REPLCommand
pHelp = symbol "help" >> return Help

pExit :: Parser REPLCommand
pExit = (symbol "q" <|> symbol "quit" <|> symbol "exit") >> return Exit
