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

-- parsing sequents
pSequent :: Parser Sequent
pSequent = pSeqBoth +++ pSeqHead +++ pSeqBody
           <?> "sequent"

xpSequent :: Parser Sequent
xpSequent = xpSeqBoth +++ xpSeqHead +++ xpSeqBody
            <?> "sequent"

-- sequents with body and head
pSeqBoth :: Parser Sequent
pSeqBoth = do {whiteSpace; 
               b <- pBody; 
               reservedOp "=>";
               h <- pHead;
               eof;
               return (Sequent b h)}

xpSeqBoth :: Parser Sequent
xpSeqBoth = do {whiteSpace; 
                b <- xpBody; 
                reservedOp "=>";
                h <- xpHead;
                eof;
                return (Sequent b h)}

-- headless sequents
pSeqBody :: Parser Sequent
pSeqBody = do {whiteSpace;
               reservedOp "~";
               b <- pBody;
               eof;
               return (Sequent b Fls)}

xpSeqBody :: Parser Sequent
xpSeqBody = do {whiteSpace;
                reservedOp "~";
                b <- xpBody;
                eof;
                return (Sequent b Fls)}

-- bodyless sequents
pSeqHead :: Parser Sequent
pSeqHead = do {whiteSpace;
               b <- pHead;
               eof;
               return (Sequent Tru b)}

xpSeqHead :: Parser Sequent
xpSeqHead = do {whiteSpace;
                b <- xpHead;
                eof;
                return (Sequent Tru b)}

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
         
-- scans initial whitespace and tests that input string is exhausted
pBodyFormula :: Parser Formula
pBodyFormula = do { whiteSpace; f <- pBody; eof; return f }

pHeadFormula :: Parser Formula
pHeadFormula = do { whiteSpace; f <- pHead; eof; return f }

-- Like pFormula but over the extended language
xpBodyFormula :: Parser Formula
xpBodyFormula = do { whiteSpace; f <- xpBody; eof; return f }

xpHeadFormula :: Parser Formula
xpHeadFormula = do { whiteSpace; f <- xpHead; eof; return f }

pBody  = pConjunctive
xpBody = xpConjunctive

pHead  = Expr.buildExpressionParser disjTable pHeadFactor
       <?> "head formula"
       
xpHead  = Expr.buildExpressionParser disjTable xpHeadFactor
       <?> "head formula"

pConjunctive = Expr.buildExpressionParser conjTable pConjFactor
             <?> "conjunctive formula"
xpConjunctive = Expr.buildExpressionParser conjTable xpConjFactor
              <?> "conjunctive formula"

-- | Operator table for formulas.
table = [ [binOp "&" And Expr.AssocLeft]
        , [binOp "|" Or Expr.AssocLeft] ]
        where preOp op f = Expr.Prefix (do { reservedOp op; return f })
              binOp op f = Expr.Infix (do { reservedOp op; return f })
                                      
conjTable = [ [binOp "&" And Expr.AssocLeft] ]
    where binOp op f = Expr.Infix (do { reservedOp op; return f })

disjTable = [ [binOp "|" Or Expr.AssocLeft] ]
    where binOp op f = Expr.Infix (do { reservedOp op; return f })

-- change the last line to return (f vars fmla) if want to allow quantifiers
-- to bind lists of vars
pExistential :: Parser Formula
pExistential = (quantWithFn "exists" Exists <|> quantWithFn "Exists" Exists)
               +++ (quant "exists" Exists <|> quant "Exists" Exists)
               +++ pConjunctive
    where quant q f =  do
                        reserved q
                        vars <- many1 identifier
                        let vars' = map Variable vars
                        reservedOp "."
                        fmla <- pConjunctive
                        return $ (foldr ($) fmla ((f Nothing) <$> vars'))
          quantWithFn q f =  do
                        reserved q
                        fn <- identifier
                        vars <- many1 identifier
                        let vars' = map Variable vars
                        reservedOp "."
                        fmla <- pConjunctive
                        return $ (foldr ($) fmla ((f (Just fn)) <$> vars'))

xpExistential :: Parser Formula
xpExistential = (quantWithFn "exists" Exists <|> quantWithFn "Exists" Exists)
                +++ (quant "exists" Exists <|> quant "Exists" Exists)
                +++ xpConjunctive
    where quant q f =  do
                        reserved q
                        vars <- many1 identifier
                        let vars' = map Variable vars
                        reservedOp "."
                        fmla <- xpConjunctive
                        return $ (foldr ($) fmla ((f Nothing) <$> vars'))
          quantWithFn q f =  do
                        reserved q
                        fn <- identifier
                        vars <- many1 identifier
                        let vars' = map Variable vars
                        reservedOp "."
                        fmla <- xpConjunctive
                        return $ (foldr ($) fmla ((f (Just fn)) <$> vars'))


pFactor :: Parser Formula
pFactor = parens pFmla
        <|> pFls
        <|> pTru
        <|> pExistential
        <|> (pEql +++ pAtom)
        <?> "formula"

xpFactor :: Parser Formula
xpFactor = parens xpFmla
        <|> pFls
        <|> pTru
        <|> xpExistential
        <|> (xpEql +++ xpAtom)
        <?> "formula"

-- | factors of a formula.
pConjFactor = parens pConjunctive
           <|> pFls
           <|> pTru
           <|> (pEql +++ pAtom)
           <?> "formula"

-- | Like pFactor but over the extended language
xpConjFactor :: Parser Formula
xpConjFactor = parens xpConjunctive
          <|> pFls
          <|> pTru
          <|> (xpEql +++ xpAtom)
          <?> "formula"

pHeadFactor :: Parser Formula
pHeadFactor = parens pHead
           <|> pFls
           <|> pExistential
           <?> "formula"

xpHeadFactor :: Parser Formula
xpHeadFactor = parens pHead
           <|> pFls
           <|> xpExistential
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
