{-|
  Razor
  Module      : REPL.Syntax
  Description : This module defines the User Syntax for the repl. 
  Maintainer  : Salman Saghafi, Ryan Danas
-}

module REPL.Syntax where
import Control.Applicative
import Syntax.IGeometric
import Syntax.ITerm
import Syntax.GeometricParser
import Text.ParserCombinators.Parsec hiding ( (<|>) )
import qualified Text.ParserCombinators.Parsec.Expr as Expr

data Command = Go Explore | Ask Question | Display Thing | Other Utility
data Thing = TheTheory | TheModel
data Explore = Next | Augment Formula
data Question = Name Bool Term | Blame Formula
data Utility = Help | Exit

helpCommand :: String
helpCommand = 
  "-------------" ++ "\n" ++ 
	"-- Display --" ++ "\n" ++ 
  "-------------" ++ "\n" ++ 
  "  !t: show the input theory" ++ "\n" ++ 
  "  !m: show the current model" ++ "\n" ++ 
  "-----------------" ++ "\n" ++ 
  "-- Exploration --" ++ "\n" ++ 
  "-----------------" ++ "\n" ++
  "  next: show the next minimal model if available" ++ "\n" ++ 
  "  aug [seq]: Not Implemented" ++ "\n" ++ 
  "    //example: ???" ++ "\n" ++ 
  "-----------------" ++ "\n" ++ 
  "-- Explanation --" ++ "\n" ++ 
  "-----------------" ++ "\n" ++
  "  origin [elm]: display the sequents responsible for the existence of the specified element" ++ "\n" ++ 
  "    //example: origin e#7" ++ "\n" ++ 
  "  origin* [elm]: recursively displays element origins down to the ground facts, starting with the specified element" ++ "\n" ++ 
  "    //example: origin* e#42" ++ "\n" ++ 
  "  blame [fact]: display the sequents responsible for making the given fact true" ++ "\n" ++ 
  "    //example: blame Student(e^7)" ++ "\n" ++ 
  "---------------------" ++ "\n" ++ 
  "-- Other Utilities --" ++ "\n" ++ 
  "---------------------" ++ "\n" ++
  "  Type 'q' or 'quit' or 'exit' to close the REPL\n"

parseCommand :: String -> Maybe Command
parseCommand input = 
	let pResult = parse pCommand "parsing user command" input
	in case pResult of
		Left _ -> Nothing
		Right val -> Just val

pCommand :: Parser Command
pCommand = pOther +++ pDisplay +++ pAsk +++ pGo

{-|||||||||||
|| Display ||
|||||||||||-}
pDisplay :: Parser Command
pDisplay = do
  symbol "!"
  Display <$> pThing

pThing :: Parser Thing
pThing = pTheTheory +++ pTheModel

pTheTheory :: Parser Thing
pTheTheory = symbol "t" >> return TheTheory

pTheModel :: Parser Thing
pTheModel = symbol "m" >> return TheModel

{-|||||||||||||||
|| Exploration ||
|||||||||||||||-}
pGo :: Parser Command
pGo = Go <$> pExplore

pExplore :: Parser Explore
pExplore = pNext <|> pAugment

pNext :: Parser Explore
pNext = symbol "next" >> return Next

pAugment :: Parser Explore
pAugment = do 
	symbol "aug"
	Augment <$> pFormula

{-|||||||||||||||
|| Explanation ||
|||||||||||||||-}
pAsk :: Parser Command
pAsk = Ask <$> pQuestion

pQuestion :: Parser Question
pQuestion = pName <|> pBlame

pName :: Parser Question
pName = pNameHead +++ pNameRec

pNameHead :: Parser Question
pNameHead = do
  symbol "origin"
  Name False <$> pElement

pNameRec :: Parser Question
pNameRec = do
  symbol "origin*"
  Name True <$> pElement

pBlame :: Parser Question
pBlame = do
  symbol "blame"
  Blame <$> xpAtom

{-|||||||||||||
|| Utilities ||
|||||||||||||-}
pOther :: Parser Command
pOther = Other <$> pUtility

pUtility :: Parser Utility
pUtility = pHelp <|> pExit

pHelp :: Parser Utility
pHelp = symbol "help" >> return Help

pExit :: Parser Utility
pExit = (symbol "q" <|> symbol "quit" <|> symbol "exit") >> return Exit