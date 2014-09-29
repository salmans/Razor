-- vim:sw=2:ts=2:expandtab:autoindent
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
   Module      :  Math.SMT.Yices.Parser
   Copyright   :  (c) 2009 by Ki Yung Ahn
   License     :  BSD3

   Maintainer  :  Ahn, Ki Yung <kya@pdx.edu>
   Stability   :  provisional
   Portability :  portable

   Parser for the yices syntax.  Yet incomplete since it does not include
   bit vectors, and not heavily tested at all.
   See <http://yices.csl.sri.com/language.shtml> for details.
 -}
module Yices.Parser (
 typY, expY, cmdY, parseTypY, parseExpY, parseExpYs, parseCmdY
 ) where

import Yices.Syntax
import Control.Monad
import Data.Maybe
import Data.Ratio
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T

-- | parse a string of yices type format
parseTypY :: String -> TypY
parseTypY s = case parse typY "parseTypY" s of Right r  -> r
                                               Left msg -> error $ show msg
-- | parse a string of yices expression format
parseExpY :: String -> ExpY
parseExpY s = case parse expY "parseExpY" s of Right r -> r
                                               Left msg -> error $ show msg

-- | parse a string of many yices expressions
parseExpYs :: String -> [ExpY]
parseExpYs s = case parse expYs "parseExpYs" s of Right r -> r
                                                  Left msg -> error $ show msg
 
-- | parse a string of yices command format
parseCmdY :: String -> CmdY
parseCmdY s = case parse cmdY "parseCmdY" s of Right r -> r
                                               Left msg -> error $ show msg


tp = T.makeTokenParser emptyDef

stringLiteral = T.stringLiteral tp 

lexme p = do spaces
             r <- p
             spaces
             return r

paren = between (lexme $ char '(') (lexme $ char ')')

many2 p = liftM2 (:) p (many1 p)

idStart = identStart emptyDef

idLetter = identLetter emptyDef

ident = liftM2 (:) idStart (many idLetter)

identifier = lexme ident <?> "identifier"


neg = char '-' >> return negate


nat = liftM (read :: String -> Integer) (many1 digit)

int = option id neg `ap` nat

integer = lexme int <?> "integer"


rat = do n <- nat
         char '/'
         d <- nat
         return (n % d)

ratio = option id neg `ap` rat

rational = lexme ratio <?> "rational"


true = string "true" >> return True

false = string "false" >> return False

bool = true <|> false

boolean = lexme bool <?> "boolean"

tok = try . lexme . string

-- | parsec parser for yices types
typY :: GenParser Char st TypY
typY = typYsimple <|> paren typYinParen
typYinParen = subtype <|> subrange <|> arr <|> tup <|> rec
           <|> datatype <|> scalar

vart = liftM VarT identifier

typYsimple = do x <- identifier
                mt <- optionMaybe $ tok "::" >> (vart <|> paren typYinParen)
                return $ case mt of Nothing -> VarT x
                                    Just t  -> DEP (x,t)

subtype = tok "subtype" >> liftM2 SUBTYPE (paren idty) expY
subrange = tok "subrange" >> liftM2 SUBRANGE expY expY
arr = tok "->" >> liftM ARR (many2 typY)
tup = tok "tuple" >> liftM TUP (many2 typY)
rec = tok "record" >> liftM REC (many1 idty)
datatype = tok "datatype" >> liftM DATATYPE (many1 $ paren ctordef)
scalar = tok "scalar" >> liftM SCALAR (many1 identifier)

ctordef = liftM2 (,) identifier (many idty)

idty = do x <- identifier
          tok "::"
          t <- typY
          return (x,t)

idex = do x <- identifier
          tok "::"
          e <- expY
          return (x,e)



-- | parsec parser for yices expressions
expY :: GenParser Char st ExpY
expY = try litb <|> try litr <|> liti <|> vare <|> paren expYinParen

expYs :: GenParser Char st [ExpY]
expYs = many expY

expYinParen = and_ <|> or_ <|> not_ <|> try (=>:) <|> (=:) <|> (/=:)
          <|> try (<:) <|> (<=:) <|> try (>:) <|> (>=:)
          <|> (+:) <|> (-:) <|> (*:) <|> (/:) <|> div_ <|> mod_
          <|> if_ <|> ite <|> let_ <|> forall <|> exists
          <|> lambda <|> mktup <|> mkrec <|> update <|> select
          <|> app

vare = liftM VarE identifier
litb = liftM LitB boolean
liti = liftM LitI integer
litr = liftM LitR rational

and_ = tok "and" >> liftM AND (many1 expY)
or_ = tok "or" >> liftM OR (many1 expY)
not_ = tok "not" >> liftM NOT expY
(=:) = tok "=" >> liftM2 (:=) expY expY
(/=:) = tok "/=" >> liftM2 (:/=) expY expY
(=>:) = tok "=>" >> liftM2 (:=>) expY expY
(<:) = tok "<" >> liftM2 (:<) expY expY
(<=:) = tok "<=" >> liftM2 (:<=) expY expY
(>:) = tok ">" >> liftM2 (:>) expY expY
(>=:) = tok ">=" >> liftM2 (:>=) expY expY
(+:) = tok "+" >> liftM2 (:+:) expY expY
(-:) = tok "-" >> liftM2 (:-:) expY expY
(*:) = tok "*" >> liftM2 (:*:) expY expY
(/:) = tok "/" >> liftM2 (:/:) expY expY
div_ = tok "div" >> liftM2 DIV expY expY
mod_ = tok "mod" >> liftM2 MOD expY expY
if_ = tok "if" >> liftM3 IF expY expY expY
ite = do tok "ite" >> liftM3 ITE expY expY expY
let_ = tok "let" >> liftM2 LET (paren $ many1 binding) expY
forall = tok "forall" >> liftM2 FORALL (paren $ many1 idty) expY
exists = tok "exists" >> liftM2 EXISTS (paren $ many1 idty) expY
lambda = tok "lambda" >> liftM2 LAMBDA (paren $ many1 idty) expY
mktup = tok "mk-tuple" >> liftM MKTUP (many2 expY)
mkrec = tok "mk-record" >> liftM MKREC (many1 idex)
update = do tok "update"
            e <- expY
            ix <- liftM FunIx (paren $ many expY) <|>
                  liftM TupIx integer <|>
                  liftM RecIx identifier
            v <- expY
            return $ case ix of FunIx es -> UPDATE_F e es v
                                TupIx i  -> UPDATE_T e i  v
                                RecIx f  -> UPDATE_R e f  v
select = do tok "select"
            e <- expY
            ix <- liftM TupIx integer <|>
                  liftM RecIx identifier
            return $ case ix of TupIx i -> SELECT_T e i
                                RecIx f -> SELECT_R e f
app = liftM2 APP expY (many1 expY)

binding = do x <- identifier
             mt <- optionMaybe $ tok "::" >> (vart <|> paren typYinParen)
             e <- expY
             return ((x,mt),e)

data IndexType = FunIx [ExpY] | TupIx Integer | RecIx String


-- | parsec parser for yices command
cmdY :: GenParser Char st CmdY
cmdY = paren $ try deftyp <|> define <|> try assert <|> assertp <|> retract <|>
               check <|> maxsat <|> sete <|> setv <|> setao <|> push <|> pop <|>
               echo <|> include <|> status <|> dump <|> exit

deftyp = tok "define-type" >> liftM2 DEFTYP identifier (optionMaybe typY)
define = tok "define" >> liftM2 DEFINE idty (optionMaybe expY)
assert = tok "assert" >> liftM ASSERT expY
assertp = tok "assert+" >> liftM2 ASSERT_P expY (optionMaybe integer)
retract = tok "retract" >> liftM RETRACT integer
check = tok "check" >> return CHECK
maxsat = tok "max-sat" >> return MAXSAT
sete = tok "set-evidence!" >> liftM SETE boolean
setv = tok "set-verbosity!" >> liftM SETV integer
setao = tok "set-arith-only!" >> liftM SETAO boolean
push = tok "push" >> return PUSH
pop = tok "pop" >> return POP
echo = tok "echo" >> liftM ECHO stringLiteral
include = tok "include" >> liftM ECHO stringLiteral
reset = tok "reset" >> return RESET
status = tok "status" >> return DUMP
dump = tok "dump-context" >> return DUMP
exit = tok "exit" >> return EXIT

