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

lexer :: TokenParser ()
lexer = Token.makeTokenParser $ haskellStyle
    { Token.reservedOpNames = [ "~", "&", "|", "=>", "<=>", "."]
    , Token.reservedNames = [ "forall", "Forall", "exists", "Exists"
                            , "Truth", "Falsehood" ]
    }

-- Token parsers provided by Text.Parsec.Token
whiteSpace = Token.whiteSpace lexer
symbol     = Token.symbol lexer
parens     = Token.parens lexer
identifier = Token.identifier lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
commaSep   = Token.commaSep lexer