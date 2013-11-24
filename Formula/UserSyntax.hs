module Formula.UserSyntax where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding ( (<|>) )
import Text.Parsec.Token ( TokenParser )
import qualified Text.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language ( haskellStyle )
import qualified Text.ParserCombinators.Parsec.Expr as Expr

import Formula.SyntaxGeo
import Chase.Problem.Observation

-- These are for interactive mode

natural = Token.natural lexer

parseUserFact :: String -> Obs
parseUserFact input = case parse pUserFact "parsing user fact" input of
  Left err -> error (show err)
  Right val -> val

pUserFact :: Parser Obs
pUserFact = pUserEquality <|> pUserAtom

pUserEquality :: Parser Obs
pUserEquality = do
  t1 <- pUserTerm
  symbol "="
  t2 <- pUserTerm
  return $ Eql t1 t2

pUserAtom :: Parser Obs
pUserAtom = do
  name <- identifier
  termList <- pUserTermList <|> return []
  return $ Fct $ R name termList

pUserTermList :: Parser [Term]
pUserTermList = parens $ commaSep pUserTerm

pUserTerm :: Parser Term
pUserTerm = pUserElement <|> do
  name <- identifier
  pUserFunction name <|> pUserConstant name <?> "user term"

pUserElement :: Parser Term
pUserElement = do
  symbol "e#"
  n <- natural
  return $ Elm $ "e#" ++ show n

pUserFunction :: String -> Parser Term
pUserFunction name = pTermList >>= return . Fn name

pUserConstant :: String -> Parser Term
pUserConstant name = return $ Var name
