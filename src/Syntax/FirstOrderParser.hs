{-| 
  Razor
  Module      : Syntax.FirstOrderParser
  Description : Defines the lexer and the tokens used for parsing first-order 
  syntax.
  Maintainer  : Salman Saghafi -}

module Syntax.FirstOrderParser ( lexer, whiteSpace, symbol, parens, identifier
                               , reserved, reservedOp, commaSep, natural) where

import Syntax.IFirstOrderParser