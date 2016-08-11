{- This file is part of Razor.

  Razor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Razor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Razor.  If not, see <http://www.gnu.org/licenses/>.

  Module      : Syntax.GeometricParser
  Description : Defines the lexer and the tokens used for parsing geometric 
  syntax.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Syntax.GeometricParser ( lexer, whiteSpace, symbol, parens, identifier
                              , reserved, reservedOp, commaSep) where

import Text.Parsec.Token ( TokenParser )
import qualified Text.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

import Text.Parsec.Language

def :: LanguageDef ()
def = emptyDef{ commentStart          = "{-"
              , commentEnd            = "-}"
              , commentLine           = "--"
              , identStart            = letter <|> char '_'
              , identLetter           = alphaNum <|> char '_'
              , opStart               = oneOf "~|.;="
              , opLetter              = oneOf "~|.;=>"
              , Token.reservedOpNames = ["&", "|", "=>", ".", ";"]
              , Token.reservedNames   = ["exists", "Exists"
                                        ,"Truth", "Falsehood"]
              , caseSensitive         = True }


lexer :: TokenParser ()
lexer = Token.makeTokenParser def

-- Token parsers provided by Text.Parsec.Token
whiteSpace = Token.whiteSpace lexer
symbol     = Token.symbol lexer
parens     = Token.parens lexer
identifier = Token.identifier lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
commaSep   = Token.commaSep lexer
