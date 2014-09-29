{-| 
  Razor
  Module      : Syntax.GeometricParser
  Description : Defines the lexer and the tokens used for parsing geometric 
  syntax.
  Maintainer  : Salman Saghafi -}

module Syntax.GeometricParser ( lexer, whiteSpace, symbol, parens, identifier
                              , reserved, reservedOp, commaSep) where

import Syntax.IGeometricParser