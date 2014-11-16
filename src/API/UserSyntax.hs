{-|
  Razor
  Module      : API.UserSyntax
  Description : This module defines the User Syntax for the repl. 
  Maintainer  : Salman Saghafi, Ryan Danas
-}

module API.UserSyntax where
import Control.Applicative
import Syntax.IGeometric
import Syntax.ITerm
import Syntax.GeometricParser
import Text.ParserCombinators.Parsec hiding ( (<|>) )
import Text.Parsec.Error
import Text.Parsec.Prim hiding ( (<|>) )
import qualified Text.ParserCombinators.Parsec.Expr as Expr

data Command = Go Explore | Ask Question | Display Thing | Other Utility | SyntaxError String
data Thing = DispTheory | DispModel
data Explore = Next | Augment Formula
data Question = Name Bool Bool Term | Blame Formula
data Utility = Help | Exit

helpCommand :: String
helpCommand = 
  "-------------" ++"\n"++ 
  "-- Display --" ++"\n"++ 
  "-------------" ++"\n"++ 
  "  !t: show the input theory" ++"\n"++ 
  "  !m: show the current model" ++"\n"++
  "-----------------" ++"\n"++ 
  "-- Exploration --" ++"\n"++ 
  "-----------------" ++"\n"++
  "  next :: show the next minimal model if available" ++"\n"++ 
  "  aug [seq] :: Not Implemented" ++"\n"++ 
  "-----------------" ++"\n"++ 
  "-- Explanation --" ++"\n"++ 
  "-----------------" ++"\n"++
  "  origin(*) [elm] :: show one theory rule responsible for the existence of the specified element" ++"\n"++
  "    * :: recursively show origins that the given element depends on" ++"\n"++ 
  "    :: examples :: origin e^1" ++"\n"++ 
  "                   origin* e^3" ++"\n"++ 
  "  origins(*) [elm] :: shows all the theory rules responsible for the existence of the specified element" ++"\n"++
  "    * :: recursively show origins that the given element depends on" ++"\n"++ 
  "    :: examples :: origins e^2" ++"\n"++ 
  "                   origins* e^4" ++"\n"++ 
  "  blame [fact] :: show the sequents responsible for making the given fact true" ++"\n"++ 
  "    :: example :: blame Student(e^7)" ++"\n"++ 
  "---------------------" ++"\n"++ 
  "-- Other Utilities --" ++"\n"++ 
  "---------------------" ++"\n"++
  "  Type 'q' or 'quit' or 'exit' to close the REPL\n"


parseCommand :: String -> Command
parseCommand input = 
	let pResult = parse pCommand "parsing user command" input
	in case pResult of
		Left err -> SyntaxError (show err)
		Right val -> val

pCommand :: Parser Command
pCommand = pOther +++ pDisplay +++ pAsk +++ pGo

{-|||||||||||
|| Display ||
|||||||||||-}
pDisplay :: Parser Command
pDisplay = do
  symbol "!"
  (Display <$> pThing) 

pThing :: Parser Thing
pThing = pDispTheory +++ pDispModel

pDispTheory :: Parser Thing
pDispTheory = symbol "t" >> return DispTheory

pDispModel :: Parser Thing
pDispModel = symbol "m" >> return DispModel

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
	Augment <$> xpAtom

{-|||||||||||||||
|| Explanation ||
|||||||||||||||-}
pAsk :: Parser Command
pAsk = Ask <$> pQuestion

pQuestion :: Parser Question
pQuestion = pName <|> pBlame

pName :: Parser Question
pName = pNameOne +++ pNameAll
pNameOne :: Parser Question
pNameOne = do
  symbol "origin"
  pNameOneHead +++ pNameOneRec
pNameOneHead :: Parser Question
pNameOneHead = do
  symbol ""
  Name False False <$> pElement
pNameOneRec :: Parser Question
pNameOneRec = do
  symbol "*"
  Name False True <$> pElement
pNameAll :: Parser Question
pNameAll = do
  symbol "origins"
  pNameAllHead +++ pNameAllRec
pNameAllHead :: Parser Question
pNameAllHead = do
  symbol ""
  Name True False <$> pElement
pNameAllRec :: Parser Question
pNameAllRec = do
  symbol "*"
  Name True True <$> pElement

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