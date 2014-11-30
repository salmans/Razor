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

data REPLCommand = Display Substate | Change Mode | ModeHelp | Help | Exit
data Substate = TheConfig | TheTheory | TheModel
data Mode = TheoryMode T.TheoryM

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
  enter <- enterMode startmode state
  case enter of
    Left err -> prettyPrint 0 ferror err
    Right state' -> runInputT defaultSettings $ loop state' startmode
  -- exit display
  displayExit

loop :: LoopMode m => REPLState -> m -> InputT IO()
loop state@(REPLState config theory gstar stream model) mode = do
  -- loop types
  let stay = loop state mode
  let go state' = loop state' mode
  -- get input
  lift $ putStr "\n"
  minput <- getInputLine "% "
  -- parse input into a command and act depending on the case
  case minput of
      Nothing -> return ()
      Just cmd -> do
        case parseREPLCommand cmd of
          -- Try and run the mode command
          Nothing -> do
            run <- lift $ runOnce mode state cmd
            case run of
              Left err -> do
                lift $ prettyPrint 0 ferror err
                stay
              Right state' -> go state'
          -- Run the overall REPL command
          Just command -> case command of
            Display substate -> case substate of
              TheConfig -> lift (putStrLn (show config)) >> stay
              TheTheory -> lift (putStrLn (show theory)) >> stay
              TheModel -> lift (putStrLn (show model)) >> stay
            Change mode -> case mode of
              TheoryMode m -> lift (putStrLn "no chmod yet") >> stay
            ModeHelp -> lift (showHelp mode) >> stay
            Help -> lift replHelp >> stay
            Exit -> return ()

------------------------
-- Main REPL Commands --
------------------------
replHelp :: IO()
replHelp = putStrLn "repl help"

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
pMode = pTheoryM

pTheoryM :: Parser Mode
pTheoryM = symbol "t" >> return (TheoryMode T.TheoryM)

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
