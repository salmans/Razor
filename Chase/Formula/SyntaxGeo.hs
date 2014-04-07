{-|Time-stamp: <2013-02-11 23:59:05 Salman Saghafi>

Data type definitions of Geometric Logic. This is very similar to SyntaxFol but there are a few major differences:
    1. Geometric formulas do not have negations in them.
    2. Geometric sequents are separate data structures.
    3. Universal quantification is permitted only at the level of sequents.

-}
module Chase.Formula.SyntaxGeo where

import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe
import Control.Exception -- for assert
import Debug.Trace
import Data.List(intercalate)

--import Text.PrettyPrint.HughesPJ((<+>), (<>))
--import qualified Text.PrettyPrint.HughesPJ as PP

import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec hiding ( (<|>) )
import Text.Parsec.Token ( TokenParser )
import qualified Text.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language ( haskellStyle )
import qualified Text.ParserCombinators.Parsec.Expr as Expr
--

import Data.Set ((\\))

--
-- Terms and Formulas
--
type Var = String
type Vars = [Var]
type Sym = String
newtype Elem = Elem Sym -- elements of the domain
    deriving (Eq, Ord, Read) -- read for user input

data Term = Var Var
          | Elm Elem -- elements of the domain are terms
          | Fn Sym [Term]
          | Rn Sym [Term] -- Just like Fn but for cases where we treat relations
                          -- like functions.
            -- Salman: this is not idea!
            deriving (Eq, Ord, Read) -- read for user input
            --deriving (Eq, Ord, Show)

data Atom = R Sym [Term]
          | F Sym [Term] -- Just like R but for cases where we treat functions
                         -- like relations.
           deriving (Eq, Ord, Read) -- read for user input

data Formula = Tru
             | Fls
             | Atm Atom
             | And Formula Formula
             | Or Formula Formula
             | Exists Var Formula
             deriving (Ord, Eq)
             -- deriving (Ord, Eq, Show)

instance Show Elem where show = prettyElem

instance Show Atom where show = prettyAtom

instance Show Formula where show = prettyFormula

instance Show Term where show = prettyTerm


prettyFormula :: Formula -> String
prettyFormula fmla = case fmla of
     Tru ->  "Truth"
     Fls  ->  "Falsehood"
     Atm t -> show t
     (And a b)  ->  "(" ++ (prettyFormula a) ++ " & " ++ (prettyFormula b) ++ ")"
     (Or a b)  ->  "(" ++ (prettyFormula a) ++ " | " ++ (prettyFormula b) ++ ")"
     (Exists v f)  ->  "exists " ++ v ++ ". " ++ (prettyFormula f)


prettyTerm :: Term -> String
prettyTerm t = case t of
     (Fn f ts) -> f ++ "(" ++ (intercalate "," (map prettyTerm ts)) ++ ")"
     (Rn f ts) -> f ++ "(" ++ (intercalate "," (map prettyTerm ts)) ++ ")"
     (Var v) -> v
     (Elm e) -> prettyElem e

prettyAtom :: Atom -> String
prettyAtom (R "=" [t1,t2]) = "(" ++ (prettyTerm t1) ++ 
                             " = " ++ (prettyTerm t2) ++ ")"
prettyAtom (R sym ts) = sym ++ "(" ++ (intercalate "," (map prettyTerm ts)) ++ ")"
prettyAtom (F sym ts) = sym ++ "(" ++ (intercalate "," (map prettyTerm ts)) ++ ")"

prettyElem :: Elem -> String
prettyElem (Elem e) = "{" ++ e ++ "}"

--
-- Some pretty-printing tools
--
flattenListString :: String -> String
flattenListString  = filter (\c -> c /= '[' && c /= ']' && c /= '\"')

-- | Use this to strip off "[" and "]" and remove "\" infront of chars
unbracket :: String -> String
unbracket cs = init $ tail (filter (\c -> c /= '\"') cs)

-- | Make a list of strings look like a set
swapSquareCurly :: String -> String
swapSquareCurly ss = let bareList = unbracket ss
                     in "{" ++ bareList ++ "}"

-- | Shows a formula with a binary op as the outermost structure.
showBinFormula :: String -> Formula -> Formula -> String
showBinFormula op f g = f' ++ " " ++ op ++ " " ++ g'
    where f' = addParens $ show f
          g' = addParens $ show g

-- | Surrounds the string with parens.
addParens :: String -> String
addParens s = "(" ++ s ++ ")"

-- | Delimit list contents with commas.
commas :: [String] -> String
commas = intercalate ","

-- | Shows the list, comma separated and surrounded with parens instead of
-- brackets.
showWithParens :: Show t => [t] -> String
showWithParens = addParens . intercalate ", " . map show


{-
 =====================================
 Parsing
 ======================================
-}

-- backtracking choice combinator
infixr 5 +++
(+++)             :: Parser a -> Parser a -> Parser a
p +++ q = ( Text.ParserCombinators.Parsec.try p) <|> q

-- Lexing

-- | The lexer. Using haskellStyle allows for haskell-style comments.
lexer :: TokenParser ()
lexer = Token.makeTokenParser $ haskellStyle
    { Token.reservedOpNames = [ "~", "&", "|", "=>", "<=>", ".", "#"]
    , Token.reservedNames = [ "forall", "Forall", "exists", "Exists" , "Truth", "Falsehood" ]
    }

-- Token parsers provided by Text.Parsec.Token
whiteSpace = Token.whiteSpace lexer
symbol = Token.symbol lexer
parens = Token.parens lexer
identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
commaSep = Token.commaSep lexer
-- many = Token.many lexer
stringLiteral = Token.stringLiteral lexer
decimal = Token.decimal lexer
brackets = Token.brackets lexer
dot = Token.dot lexer
semiSep1 = Token.semiSep1 lexer

-- Parsing Terms
parseTerm :: String -> Term
parseTerm input =
 let pResult =  parse pTerm "parsing Term" input
    in case pResult of
           Left err -> error (show err)
           Right val -> val

pTerm :: Parser Term
-- Parses the identifier and then lets the next thing decide whether its a
-- variable name or a function name (depending on whether there is an argument
-- list).
pTerm = (pModelElement <|> (identifier >>= pTermWithIden))
    <?> "term"

pModelElement :: Parser Term
pModelElement = do
  reservedOp "#"
  number <- decimal
  return $ Elm $ Elem ("e#" ++ show number)

-- | Parser for terms with leading identifier already parsed. This is the "next
-- thing" that decides whether the given string is a variable name or a function
-- name.
pTermWithIden :: String -> Parser Term
pTermWithIden name
    = Fn name <$> pTermList
    <|> return (Var name)

-- A list of terms separated by commas.
pTermList :: Parser [Term]
pTermList = (parens $ commaSep pTerm)
         <?> "list of terms"



parseFormula :: String -> Formula
parseFormula input =
 let pResult =  parse pFormula "parsing Formula" input
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
table = [ [binOp "&" And Expr.AssocLeft]
        , [binOp "|" Or Expr.AssocLeft]
        --, [binOp "=>" Imp Expr.AssocRight, binOp "<=>" Iff Expr.AssocRight]
        ]
    where preOp op f = Expr.Prefix (do { reservedOp op; return f })
          binOp op f = Expr.Infix (do { reservedOp op; return f })


-- change the last line to return (f vars fmla) if want to allow quantifiers
-- to bind lists of vars
pQuantified :: Parser Formula
pQuantified = quant "exists" Exists <|> quant "Exists" Exists
    where quant q f =  do
                        reserved q
                        vars <- many1 identifier
                        reservedOp "."
                        fmla <- pFmla
                        return $ (foldr ($) fmla (map f vars))
                        --return (f vars fmla)

-- | factors of a formula.
pFactor :: Parser Formula
pFactor = parens pFmla
          <|> pTru
          <|> pFls
          <|> pQuantified
          <|> (Atm <$> pAtomic)
          -- <|> pAtom
          <?> "formula"

pAtomic :: Parser Atom
pAtomic = pEql +++ pAtom

-- | Parser for atomic formulas. Works like parsing a term, except that it is an
-- atom either way. Atoms with no arguments need not provide parens, however.
pAtom :: Parser Atom
pAtom = identifier >>= pAtomWithIden
     <?> "atom"

-- | Parser for atomic formulas with the leading identifier already parsed.
pAtomWithIden :: Sym -> Parser Atom
pAtomWithIden name = R name <$> pTermList
                    <|>
                    (return $ R name [])

{-
makeAtom :: Sym -> [Term] -> Formula
makeAtom  x y = Atm (R x y)
-}

-- |  equalities
pEql :: Parser Atom
pEql = do {t1 <- pTerm; (symbol "=") ; t2 <- pTerm; return (R "=" [t1, t2])}

pTru :: Parser Formula
pTru  = do { reserved "Truth";   return Tru }

pFls :: Parser Formula
pFls  = do {  reserved "Falsehood";   return Fls}


{-| Sequent is a data structure for geometric sequents.
  - sequentBody: the formula on right of the sequent
  - sequentHead: the formula on left of the sequent
-}
data Sequent = Sequent {
      sequentBody :: Formula,
      sequentHead :: Formula
}

instance Show Sequent where
    show (Sequent b h) = (show b) ++ " => " ++ (show h)

{-| A theory is simply a list of sequents. -}
type Theory = [Sequent]

parseSequent :: String -> Sequent
parseSequent input =
 let pResult =  parse pSequent "parsing sequent" input
    in case pResult of
           Left err -> error (show err)
           Right val -> val

pSequent :: Parser Sequent
pSequent = pSeqBoth +++ pSeqLeft +++ pSeqRight
           <?> "sequent"

pSeqBoth :: Parser Sequent
pSeqBoth = do {whiteSpace; 
               b <- pFmla; 
               reservedOp "=>";
               h <- pFmla;
               return (Sequent b h)}

pSeqLeft :: Parser Sequent
pSeqLeft = do {whiteSpace;
               reservedOp "~";
               b <- pFmla;
               return (Sequent b Fls)}

pSeqRight :: Parser Sequent
pSeqRight = do {whiteSpace;
                b <- pFmla;
                return (Sequent Tru b)}


-- Query language stuff

data Command = Display ModelExpr | Assign String ModelExpr | Exit

data ModelExpr = ThyLiteral Theory | LoadFromFile String
               | ApplyOp ModelExpr ModelOperation | LastResult | ModelVar String

data ModelOperation = AddConstraint Atom | RemoveConstraint | NextModel
                    | PreviousModel | Origin

pCommand :: Parser Command
pCommand = pExit <|> (pAssign +++ pDisplay)

pDisplay :: Parser Command
pDisplay = Display <$> pModelExpr

pAssign :: Parser Command
pAssign = do
  var <- identifier
  symbol ":="
  expr <- pModelExpr
  return $ Assign var expr

pExit :: Parser Command
pExit = symbol "exit" >> return Exit

pModelExpr :: Parser ModelExpr
pModelExpr = pExplicitModelExpr <|> pImpliedOp +++ pImpliedAdd

pExplicitModelExpr :: Parser ModelExpr
pExplicitModelExpr = do
  base <- pThyLiteral <|> pLoadFromFile <|> pLastResult <|> pModelVar
  ops <- many $ dot >> pModelOperation
  return $ foldl ApplyOp base ops

pImpliedOp :: Parser ModelExpr
pImpliedOp = foldl ApplyOp LastResult <$> sepBy1 pModelOperation dot

pImpliedAdd :: Parser ModelExpr
pImpliedAdd = ApplyOp LastResult <$> AddConstraint <$> pAtomic

pThyLiteral :: Parser ModelExpr
pThyLiteral = ThyLiteral <$> brackets (semiSep1 pSequent)

pLoadFromFile :: Parser ModelExpr
pLoadFromFile = LoadFromFile <$> stringLiteral

pLastResult :: Parser ModelExpr
pLastResult = symbol "~" >> return LastResult

pModelVar :: Parser ModelExpr
pModelVar = do
  symbol "@"
  ModelVar <$> identifier

pModelOperation :: Parser ModelOperation
pModelOperation = pAddConstraint <|> pRemoveConstraint <|> pNextModel <|> pPreviousModel <|> pOrigin

pAddConstraint :: Parser ModelOperation
pAddConstraint = do
  symbol "add"
  parens $ AddConstraint <$> pAtomic

pRemoveConstraint :: Parser ModelOperation
pRemoveConstraint = symbol "remove" >> return RemoveConstraint

pNextModel :: Parser ModelOperation
pNextModel = symbol "next" >> return NextModel

pPreviousModel :: Parser ModelOperation
pPreviousModel = symbol "previous" >> return PreviousModel

pOrigin :: Parser ModelOperation
pOrigin = symbol "origin" >> return Origin

parseCommand :: String -> Maybe Command
parseCommand input = case (parse pCommand "parsing Command" input) of
  Left _ -> Nothing
  Right val -> Just val