{-|
  Razor
  Module      : REPL.Syntax
  Description : This module defines the User Syntax for the repl. 
  Maintainer  : Salman Saghafi, Ryan Danas
-}

module REPL.Syntax where
import Control.Applicative
import Syntax.IGeometric
import Syntax.Term
import Syntax.GeometricParser
import Text.ParserCombinators.Parsec hiding ( (<|>) )
import qualified Text.ParserCombinators.Parsec.Expr as Expr

data Command = Go Explore | Ask Question | Other Utility
data Explore = Next | Augment Formula
data Question = Name Term | Blame Term
data Utility = Help | Exit

helpCommand :: String
helpCommand = 
	"Available commands..." ++ "\n" ++ 
    "  next: show the next model if available" ++ "\n" ++ 
    "  aug seq: ???" ++ "\n" ++ 
    "    //example: aug ???" ++ "\n" ++ 
    "  name elt: ???" ++ "\n" ++ 
    "    //example: name ???" ++ "\n" ++ 
    "  blame fact: ???" ++ "\n" ++ 
    "    //example: blame ???" ++ "\n" ++ 
    "  exit: close the REPL\n"

parseCommand :: String -> Maybe Command
parseCommand input = 
	let pResult = parse pCommand "parsing user command" input
	in case pResult of
		Left _ -> Nothing
		Right val -> Just val

pCommand :: Parser Command
pCommand = pOther +++ pAsk +++ pGo

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
pName = do
  symbol "name"
  Name <$> pTerm

pBlame :: Parser Question
pBlame = do
  symbol "blame"
  Blame <$> pTerm

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
pExit = symbol "exit" >> return Exit