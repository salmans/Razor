{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{- Razor
   Module      : Syntax.IGeometric
   Description : The module defines the syntax of geometric formulas, sequents
   and theories.
   Maintainer  : Salman Saghafi -}
module Syntax.IGeometric where

-- Standard
import Data.List (intercalate)

-- Control
import Control.Applicative

import Common.Basic

-- Parsec
import Text.ParserCombinators.Parsec hiding ( (<|>) )
import qualified Text.ParserCombinators.Parsec.Expr as Expr

-- Syntax
import Syntax.Term
import Syntax.GeometricParser

-- Tools
type RelSym = Sym

{-| An atomic formula is a relation applied to a list of terms. For convenience,
  we distinguish functions that have been converted to relations (in a 
  preprocessing step) as a separate case.
-}
data Atom = Rel   RelSym [Term]
          | FnRel FnSym  [Term]
          deriving (Eq, Ord)

{-| Geometric formulas are in positive existential form, having conjunctions,
 disjunctions and existential quantifiers as connectives but no negation and
 universal quantifiers. -}
{- We could use GADTs to distinguish formulas in heads of sequents from those
   in their bodies, however, we capture formula as a general datatype in case 
   we decide to relax the structured form of sequents that we have now. -}
data Formula = Tru
             | Fls
             | Atm Atom
             | And Formula Formula
             | Or  Formula Formula
             | Exists (Maybe FnSym) Variable Formula
               -- an existential quantifier is accompanied by 
               -- its Skolem function
             | Lone (Maybe FnSym) Variable Formula Formula
               -- just like Exists but requires the quantified variable to be
               -- unique in the last formula. This data constructor is used
               -- internally for translating function symbols.
             deriving (Ord, Eq)

{-| A geometric 'Sequent' is a pair of geometric 'Formula's, identified as
  the @head@ and the @body@ of the sequent. 

  [@sequentBody@] returns the body of a sequent.
  [@sequentHead@] returns the head of a sequent.
-}
data Sequent = Sequent {
      sequentBody :: Formula,
      sequentHead :: Formula
    } deriving Eq

{-| A geometric 'Theory' is a list of geometric 'Sequent's. -}
type Theory = [Sequent]


{- Show instances -}
instance Show Atom    where show = prettyAtom
instance Show Formula where show = prettyFormula
instance Show Sequent where show = prettySequent

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
prettyFormula (And p q)    =  "(" ++ (prettyFormula p) ++ 
                              " & " ++ (prettyFormula q) ++ ")"
prettyFormula (Or p q)     =  "(" ++ (prettyFormula p) ++ 
                              " | " ++ (prettyFormula q) ++ ")"
prettyFormula (Exists fn v fmla) 
    = quantName ++ show v ++ "." ++ (prettyFormula fmla)
    where quantName = case fn of
                        Nothing -> "exists "
                        Just f  -> "exists " ++ show f ++ " "
prettyFormula (Lone fn v fmla lfmla)
    = quantName ++ show v ++ "." ++ (prettyFormula fmla)
    where quantName = case fn of
                        Nothing -> "lone "
                        Just f  -> "lone " ++ show f ++ " "


prettySequent :: Sequent -> String
prettySequent (Sequent b h) = case b of
  Tru -> (show h)
  _ -> (show b) ++ " => " ++ (show h)

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

xparseFormula :: String -> Formula
xparseFormula input = let pResult =  parse xpFormula "parsing XFormula" input
                      in case pResult of
                           Left err -> error (show err)
                           Right val -> val

-- scans initial whitespace and tests that input string is exhausted
pFormula :: Parser Formula
pFormula = do { whiteSpace; f <- pFmla; eof; return f }

-- Like pFormula but over the extended language
xpFormula :: Parser Formula
xpFormula = do { whiteSpace; f <- xpFmla; eof; return f }

-- | Parser for formulas.
pFmla :: Parser Formula
pFmla = Expr.buildExpressionParser table pFactor
        <?> "formula"

-- | Like pFmla but over the extended language
xpFmla :: Parser Formula
xpFmla = Expr.buildExpressionParser table xpFactor
         <?> "formula"

-- | Operator table for formulas.
table = [ [binOp "&" And Expr.AssocLeft]
        , [binOp "|" Or Expr.AssocLeft]
        --, [binOp "=>" Imp Expr.AssocRight, binOp "<=>" Iff Expr.AssocRight]
        ]
    where preOp op f = Expr.Prefix (do { reservedOp op; return f })
          binOp op f = Expr.Infix (do { reservedOp op; return f })


-- change the last line to return (f vars fmla) if want to allow quantifiers
-- to bind lists of vars
pQuantified :: Parser Formula
pQuantified = (quantWithFn "exists" Exists <|> quantWithFn "Exists" Exists)
              +++ (quant "exists" Exists <|> quant "Exists" Exists)
    where quant q f =  do
                        reserved q
                        vars <- many1 identifier
                        let vars' = map Variable vars
                        reservedOp "."
                        fmla <- pFmla
                        return $ (foldr ($) fmla ((f Nothing) <$> vars'))
          quantWithFn q f =  do
                        reserved q
                        fn <- identifier
                        vars <- many1 identifier
                        let vars' = map Variable vars
                        reservedOp "."
                        fmla <- pFmla
                        return $ (foldr ($) fmla ((f (Just fn)) <$> vars'))

xpQuantified :: Parser Formula
xpQuantified = (quantWithFn "exists" Exists <|> quantWithFn "Exists" Exists)
               +++ (quant "exists" Exists <|> quant "Exists" Exists)
    where quant q f =  do
                        reserved q
                        vars <- many1 identifier
                        let vars' = map Variable vars
                        reservedOp "."
                        fmla <- xpFmla
                        return $ (foldr ($) fmla ((f Nothing) <$> vars'))
          quantWithFn q f =  do
                        reserved q
                        fn <- identifier
                        vars <- many1 identifier
                        let vars' = map Variable vars
                        reservedOp "."
                        fmla <- xpFmla
                        return $ (foldr ($) fmla ((f (Just fn)) <$> vars'))

-- | factors of a formula.
pFactor :: Parser Formula
pFactor = parens pFmla
          <|> pTru
          <|> pFls
          <|> pQuantified
          <|> (pEql +++ pAtom)
          -- <|> pAtom
          <?> "formula"

-- | Like pFactor but over the extended language
xpFactor :: Parser Formula
xpFactor = parens pFmla
          <|> pTru
          <|> pFls
          <|> xpQuantified
          <|> (xpEql +++ xpAtom)
          -- <|> pAtom
          <?> "formula"

-- | Parser for atomic formulas. Works like parsing a term, except that it is an
-- atom either way. Atoms with no arguments need not provide parens, however.
pAtom :: Parser Formula
pAtom = identifier >>= pAtomWithIden
     <?> "atom"

-- | Like pAtom but over the extended language
xpAtom :: Parser Formula
xpAtom = identifier >>= xpAtomWithIden
         <?> "atom"

-- | Parser for atomic formulas with the leading identifier already parsed.
pAtomWithIden :: RelSym -> Parser Formula
pAtomWithIden name = makeAtom name <$> pTermList
                    <|>
                    (return $ Atm (Rel name []))

-- | Like pAtomWithIden but over the extended langauge
xpAtomWithIden :: RelSym -> Parser Formula
xpAtomWithIden name = makeAtom name <$> xpTermList
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

-- | Like pEql but over the extended language
xpEql :: Parser Formula
xpEql = do { t1 <- xpTerm
           ; (symbol "=") 
           ; t2 <- xpTerm
           ; return (makeAtom "=" [t1, t2])}

pTru :: Parser Formula
pTru  = do { reserved "Truth";   return Tru }

pFls :: Parser Formula
pFls  = do {  reserved "Falsehood";   return Fls}


parseSequent :: String -> Sequent
parseSequent input =
 let pResult =  parse pSequent "parsing sequent" input
    in case pResult of
           Left err -> error (show err)
           Right val -> val

xparseSequent :: String -> Sequent
xparseSequent input =
    let pResult =  parse xpSequent "parsing sequent" input
    in case pResult of
         Left err -> error (show err)
         Right val -> val

pSequent :: Parser Sequent
pSequent = pSeqBoth +++ pSeqLeft +++ pSeqRight
           <?> "sequent"

xpSequent :: Parser Sequent
xpSequent = xpSeqBoth +++ xpSeqLeft +++ xpSeqRight
            <?> "sequent"

pSeqBoth :: Parser Sequent
pSeqBoth = do {whiteSpace; 
               b <- pFmla; 
               reservedOp "=>";
               h <- pFmla;
               return (Sequent b h)}

xpSeqBoth :: Parser Sequent
xpSeqBoth = do {whiteSpace; 
                b <- xpFmla; 
                reservedOp "=>";
                h <- xpFmla;
                return (Sequent b h)}

pSeqLeft :: Parser Sequent
pSeqLeft = do {whiteSpace;
               reservedOp "~";
               b <- pFmla;
               return (Sequent b Fls)}

xpSeqLeft :: Parser Sequent
xpSeqLeft = do {whiteSpace;
                reservedOp "~";
                b <- xpFmla;
                return (Sequent b Fls)}

pSeqRight :: Parser Sequent
pSeqRight = do {whiteSpace;
                b <- pFmla;
                return (Sequent Tru b)}

xpSeqRight :: Parser Sequent
xpSeqRight = do {whiteSpace;
                 b <- xpFmla;
                 return (Sequent Tru b)}