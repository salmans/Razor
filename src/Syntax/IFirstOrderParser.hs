{-| 
  Razor
  Module      : Syntax.IFirstOrderParser
  Description : The module defines the lexer and the tokens used for parsing 
  first-order syntax.
  Maintainer  : Salman Saghafi -}

module Syntax.IFirstOrderParser where

import Text.Parsec.Token ( TokenParser )
import qualified Text.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language ( haskellStyle )


basicOpNames = [ "~", "&", "|", "=>", "<=>", "." ]
basicNames   = [ "forall", "Forall", "exists", "Exists", "Truth", "Falsehood" ] 

basicLanguage = haskellStyle 
                { Token.reservedOpNames = basicOpNames
                , Token.reservedNames   = basicNames
                }

lexer :: TokenParser ()
lexer = Token.makeTokenParser $ basicLanguage

-- Token parsers provided by Text.Parsec.Token
whiteSpace = Token.whiteSpace lexer
symbol     = Token.symbol lexer
parens     = Token.parens lexer
identifier = Token.identifier lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
commaSep   = Token.commaSep lexer
natural    = Token.natural lexer