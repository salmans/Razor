{-| 
  Razor
  Module      : Syntax.IGeometricParser
  Description : The module defines the lexer and the tokens used for parsing 
  geometric syntax.
  Maintainer  : Salman Saghafi -}

module Syntax.IGeometricParser where

import Text.Parsec.Token ( TokenParser )
import qualified Text.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language ( haskellStyle )

lexer :: TokenParser ()
lexer = Token.makeTokenParser $ haskellStyle
    { Token.reservedOpNames = [ "&", "|", "=>", "."]
    , Token.reservedNames = [ "exists", "Exists", "Truth", "Falsehood" ]
    }

-- Token parsers provided by Text.Parsec.Token
whiteSpace = Token.whiteSpace lexer
symbol     = Token.symbol lexer
parens     = Token.parens lexer
identifier = Token.identifier lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
commaSep   = Token.commaSep lexer