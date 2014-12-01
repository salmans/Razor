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
import qualified REPL.Mode.Model as M
import qualified REPL.Mode.Stream as H
import qualified REPL.Mode.Stack as V
import qualified REPL.Mode.Query as Q

data REPLCommand = Display Substate | Change REPLMode | ModeHelp | Help | Exit
data Substate = TheConfig | TheTheory | TheModel
data REPLMode = ModeTheory | ModeStream | ModeStack | ModeQuery

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
loop state@(RazorState config theory _ _ model) mode stin = do
  -- get input
  minput <- getInputLine $ modeTag mode
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
              TheModel -> lift (prettyModel model) >> stay
            Change m -> case m of
              ModeTheory -> trans T.TheoryM state 
              ModeStream -> case model of
                Nothing -> eTrans M.ModelM H.StreamM 
                _ -> trans H.StreamM state 
              ModeStack -> case model of
                Nothing -> eTrans M.ModelM V.StackM
                _ -> trans V.StackM state
              ModeQuery -> case model of
                Nothing -> eTrans M.ModelM Q.QueryM
                _ -> trans Q.QueryM state
            ModeHelp -> lift (showHelp mode) >> stay
            Help -> lift replHelp >> stay
            Exit -> finish
          -- Try and run the command in the mode
          Nothing -> do
            run <- lift $ runOnce mode stin cmd
            case run of
              Left err -> lift (prettyPrint 0 ferror (err++"\n")) >> stay
              Right stout -> do
                let (state', stin') = update mode stout state
                loop state' mode stin'
  where
    -- stay in the same mode / state; no changes
    stay = loop state mode stin
    -- finish the REPL
    finish = return (state)
    -- change the REPL mode; update state as well
    trans m' s = do
      enter <- lift $ enterMode m' s
      case enter of
        Left err -> lift (prettyPrint 0 ferror err) >> stay
        Right stout -> do
          let (s', stin') = update m' stout s
          loop s' m' stin'
    -- enter an implicit mode and get the updated state without looping
    eState m' = do
      enter <- lift $ enterMode m' state
      case enter of
        Left err -> do
          lift (prettyPrint 0 ferror err)
          return Nothing
        Right stout -> do
          let (state', _) = (update m' stout state)
          return $ Just state'
    -- transition from an implicit mode state to another state
    eTrans m' m'' = do
      s' <- eState m'
      case s' of
        Nothing -> stay
        Just state' -> trans m'' state'

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
  "A model finding assistant!\n"
replHelp :: IO()
replHelp = prettyPrint 0 foutput $ ""++
  "<expr>:= | !<substate>       Display general information\n"++
  "  <substate>:=   | c           Configuration\n"++
  "                 | t           Loaded Theory\n"++
  "                 | m           Current Model\n"++
  "         | @<mode>           Transition to a different REPL Mode\n"++
  "  <mode>:=     | t             Edit Theory and Configuration\n"++
  "               | v             Vertically Explore Modelspace\n"++
  "               | h             Horizontally Explore Modelspace\n"++
  "               | q             Query Current Model\n"++
  "         | ?                 Show REPLMode Specific Help\n"++
  "         | help              Print This Message\n"++
  "         | q|quit|exit       Exit Razor\n"
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
pMode = pTheoryM <|> pStreamM <|> pStackM <|> pQueryM

pTheoryM :: Parser REPLMode
pTheoryM = symbol "t" >> return ModeTheory

pStreamM :: Parser REPLMode
pStreamM = symbol "h" >> return ModeStream

pStackM :: Parser REPLMode
pStackM = symbol "v" >> return ModeStack

pQueryM :: Parser REPLMode
pQueryM = symbol "q" >> return ModeQuery

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
