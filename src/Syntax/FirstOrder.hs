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

  Module      : Syntax.FirstOrder
  Description : The module defines the syntax of first-order formulas.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Syntax.FirstOrder ( RelSym
                         , Atom (..), Formula (..)
                         , parseFormula) where

-- import Syntax.IFirstOrder
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

  Module      : Syntax.IFirstOrder
  Description : The module defines the syntax of first-order formulas.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

--module Syntax.IFirstOrder where

-- Standard, 
import Data.List (intercalate)
import Control.Applicative

-- Parsec
import Text.ParserCombinators.Parsec hiding ( (<|>) )
import qualified Text.ParserCombinators.Parsec.Expr as Expr

-- Common
import Common.Basic

-- Syntax
import Syntax.Term ( FnSym, Variable (..), Element (..), Term
                   , pTerm, pTermList, TermBased (..))
import Syntax.FirstOrderParser



type RelSym = Sym

{-| An atomic formula is a relation applied to a list of terms. For convenience,
  we distinguish functions that have been converted to relations (in a 
  preprocessing step) as a separate case.
-}
data Atom = Rel   RelSym [Term]
          | FnRel FnSym  [Term]
          deriving (Eq, Ord)

{-| First-order formulas: negation and universal quantification is permitted. -}
data Formula = Tru
             | Fls
             | Atm    Atom
             | Not    Formula
             | And    Formula Formula
             | Or     Formula Formula
             | Imp    Formula Formula
             | Iff    Formula Formula
             | Exists (Maybe FnSym) Variable Formula 
               -- has a skolem function in it
             | Forall Variable Formula
             deriving (Ord, Eq)

{- Show instances -}
instance Show Atom    where show = prettyAtom
instance Show Formula where show = prettyFormula

{- pretty printing for atoms and formulas -}
prettyAtom :: Atom -> String
prettyAtom (Rel "=" [t1,t2])   = "(" ++ (show t1) ++ 
                                 " = " ++ (show t2) ++ ")"
prettyAtom (FnRel "=" [t1,t2]) = "(" ++ (show t1) ++ 
                                 " = " ++ (show t2) ++ ")"
prettyAtom (Rel sym ts)        = sym ++ "(" ++ 
                                 (intercalate "," (show <$> ts)) ++ ")"
prettyAtom (FnRel sym ts)      = sym ++ "(" ++ 
                                 (intercalate "," (show <$> ts)) ++ ")"

prettyFormula :: Formula -> String
prettyFormula Tru          = "Truth"
prettyFormula Fls          = "Falsehood"
prettyFormula (Atm t)      = show t
prettyFormula (Not a)      = "~" ++ (prettyFormula a)
prettyFormula (And a b)    =  "(" ++ (prettyFormula a) ++ 
                              " & " ++ (prettyFormula b) ++ ")"
prettyFormula (Or a b)     =  "(" ++ (prettyFormula a) ++ 
                              " | " ++ (prettyFormula b) ++ ")"
prettyFormula (Imp a b)    =  "(" ++ (prettyFormula a) ++ 
                              " => " ++ (prettyFormula b) ++ ")"
prettyFormula (Iff a b)    =  "(" ++ (prettyFormula a) ++ 
                              " <=> " ++ (prettyFormula b) ++ ")"
prettyFormula (Exists fn v fmla) 
                           = quantName ++ show v ++ "." ++ (prettyFormula fmla)
    where quantName = case fn of
                        Nothing -> "exists "
                        Just f  -> "exists " ++ show f ++ " "
prettyFormula (Forall v f) = "forall " ++ show v ++ "." ++ (prettyFormula f)

{- Parsing geometric formulas and sequents -}
-- backtracking choice combinator
infixr 5 +++
(+++)   :: Parser a -> Parser a -> Parser a
p +++ q = ( Text.ParserCombinators.Parsec.try p) <|> q



parseFormula :: String -> Formula
parseFormula input = let pResult =  parse pFormula "parsing Formula" input
                     in case pResult of
                          Left err -> error (show err)
                          Right val -> val

-- scans initial whitespace and tests that input string is exhausted
pFormula :: Parser Formula
pFormula = do { whiteSpace; f <- pFmla; eof; return f }

-- | Parser for formulas.
pFmla :: Parser Formula
pFmla = Expr.buildExpressionParser table pFactor
        <?> "formula"

-- | Operator table for formulas.
table = [ [preOp "~"   Not                 ]
        , [binOp "&"   And Expr.AssocLeft  ]
        , [binOp "|"   Or Expr.AssocLeft   ]
        , [binOp "=>"  Imp  Expr.AssocRight]
        , [binOp "<=>" Iff Expr.AssocRight ]
        --, [binOp "=>" Imp Expr.AssocRight, binOp "<=>" Iff Expr.AssocRight]
        ]
    where preOp op f = Expr.Prefix (do { reservedOp op; return f })
          binOp op f = Expr.Infix (do { reservedOp op; return f })


-- change the last line to return (f vars fmla) if want to allow quantifiers
-- to bind lists of vars
pQuantified :: Parser Formula
pQuantified = (qExistsWithFn "exists" Exists <|> qExistsWithFn "Exists" Exists)
              +++ (qExists "exists" Exists <|> qExists "Exists" Exists <|>
                   qForall "forall" Forall <|> qForall "Forall" Forall)
    where qExists q f =  do
                        reserved q
                        vars <- many1 identifier
                        let vars' = map Variable vars
                        reservedOp "."
                        fmla <- pFmla
                        return $ (foldr ($) fmla ((f Nothing) <$> vars'))
          qForall q f =  do
                        reserved q
                        vars <- many1 identifier
                        let vars' = map Variable vars
                        reservedOp "."
                        fmla <- pFmla
                        return $ (foldr ($) fmla (f <$> vars'))
          qExistsWithFn q f =  do
                        reserved q
                        fn <- identifier
                        vars <- many1 identifier
                        let vars' = map Variable vars
                        reservedOp "."
                        fmla <- pFmla
                        return $ (foldr ($) fmla ((f (Just fn)) <$> vars'))


-- | factors of a formula.
pFactor :: Parser Formula
pFactor = parens pFmla
          <|> pTru
          <|> pFls
          <|> pQuantified
          <|> (pEql +++ pAtom)
          <?> "formula"

-- | Parser for atomic formulas. Works like parsing a term, except that it is an
-- atom either way. Atoms with no arguments need not provide parens, however.
pAtom :: Parser Formula
pAtom = identifier >>= pAtomWithIden
     <?> "atom"

-- | Parser for atomic formulas with the leading identifier already parsed.
pAtomWithIden :: RelSym -> Parser Formula
pAtomWithIden name = makeAtom name <$> pTermList
                    <|>
                    (return $ Atm (Rel name []))

makeAtom :: RelSym -> [Term] -> Formula
makeAtom  x y = Atm (Rel x y)

-- |  equalities
pEql :: Parser Formula
pEql = do { t1 <- pTerm
          ; (symbol "=") 
          ; t2 <- pTerm
          ; return (makeAtom "=" [t1, t2])}

pTru :: Parser Formula
pTru  = do { reserved "Truth";   return Tru }

pFls :: Parser Formula
pFls  = do {  reserved "Falsehood";   return Fls}
