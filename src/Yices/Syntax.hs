-- vim:sw=2:ts=2:expandtab:autoindent
{- |
   Module      :  Math.SMT.Yices.Syntax
   Copyright   :  (c) 2009 by Ki Yung Ahn
   License     :  BSD3

   Maintainer  :  Ahn, Ki Yung <kya@pdx.edu>
   Stability   :  provisional
   Portability :  portable

   Haskell data type definition for the yices syntax.  Yet incomplete since
   it does not include bit vectors.
   See <http://yices.csl.sri.com/language.shtml> for details.
 -}
module Yices.Syntax ( TypY(..) , ExpY(..) , CmdY(..) ) where

import Data.Char
import Data.Monoid
import Data.List
import Data.Ratio
import Text.Show

-- | yices types
data TypY
  = VarT String
  | SUBTYPE (String,TypY) ExpY
  | SUBRANGE ExpY ExpY
  | ARR [TypY]
  | TUP [TypY]
  | REC [(String,TypY)]
  | DEP (String,TypY)
  | DATATYPE [(String,[(String,TypY)])]
  | SCALAR [String]
  -- BITVECTOR Integer

-- | yices expressions
data ExpY
  = VarE String
  | LitB Bool
  | LitI Integer
  | LitR Rational
  | AND [ExpY]
  | OR [ExpY]
  | NOT ExpY
  | ExpY :=> ExpY
  | ExpY := ExpY
  | ExpY :/= ExpY
  | ExpY :< ExpY
  | ExpY :<= ExpY
  | ExpY :> ExpY
  | ExpY :>= ExpY
  | ExpY :+: ExpY
  | ExpY :-: ExpY
  | ExpY :*: ExpY
  | ExpY :/: ExpY
  | DIV ExpY ExpY
  | MOD ExpY ExpY
  | IF ExpY ExpY ExpY
  | ITE ExpY ExpY ExpY
  | LET [((String,Maybe TypY),ExpY)] ExpY
  -- quantifires
  | FORALL [(String,TypY)] ExpY
  | EXISTS [(String,TypY)] ExpY
  -- functions
  | APP ExpY [ExpY]
  | UPDATE_F ExpY [ExpY] ExpY
  | LAMBDA [(String,TypY)] ExpY
  -- tuples
  | MKTUP [ExpY]
  | SELECT_T ExpY Integer
  | UPDATE_T ExpY Integer ExpY
  -- records
  | MKREC [(String,ExpY)]
  | SELECT_R ExpY String
  | UPDATE_R ExpY String ExpY
  -- bitvectors -- TODO

-- | yices declarations and commands
data CmdY
  = DEFTYP String (Maybe TypY)
  | DEFINE (String,TypY) (Maybe ExpY)
  | ASSERT ExpY
  | ASSERT_P ExpY (Maybe Integer)
  | RETRACT Integer
  | CHECK
  | MAXSAT
  | SETE Bool
  | SETV Integer
  | SETAO Bool
  | PUSH
  | POP
  | ECHO String
  | INCLUDE String
  | RESET
  | STATUS
  | DUMP
  | EXIT

paren :: ShowS -> ShowS
paren = showParen True

space = showChar ' '

showListSepByWith :: (a -> ShowS) -> String -> [a] -> ShowS
showListSepByWith showsFun sep = foldr (.) id . intersperse (showString sep) . map showsFun

showListSepBy :: (Show a) => String -> [a] -> ShowS
showListSepBy = showListSepByWith (showsPrec 0)

showStringsSepBy = showListSepByWith showString

showIdTyp, showIdVal :: Show a => (String, a) -> ShowS
showIdTyp (tname,t) = showString tname . showString "::" . showsPrec 0 t

showIdVal = showIdTyp

showCtorDef (c,[])     = showString c
showCtorDef (c,idtyps) = paren $ showString c . space . showListSepByWith showIdTyp " " idtyps

showBinding ((x,Just t),e) = paren $ showIdTyp(x,t) . space . showsPrec 0 e
showBinding ((x,Nothing),e) = paren $ showString x . space . showsPrec 0 e

instance Show TypY where
  showsPrec _ = showTypY

{-# INLINE showTypY #-}
showTypY (VarT tname) = showString tname
showTypY (SUBTYPE idty e) = paren $ showString "subtype " . showIdTyp idty . space . showsPrec 0 e
showTypY (SUBRANGE e1 e2) = paren $ showString "subrange " . showsPrec 0 e1 . space . showsPrec 0 e2
showTypY (ARR ts) = paren $ showString "-> " . showListSepBy " " ts
showTypY (TUP ts) = paren $ showString "tuple " . showListSepBy " " ts
showTypY (REC idtyps) = paren $ showString "record " . showListSepByWith showIdTyp " " idtyps
showTypY (DEP idty) = showIdTyp idty
showTypY (DATATYPE ctordefs) =
         paren $ showString "datatype " . showListSepByWith showCtorDef " " ctordefs
showTypY (SCALAR tnames) = paren $ showString "scalar " . showStringsSepBy " " tnames
  -- showsPrec _ (BITVECTOR n) = paren $ "bitvector " ++ show n

instance Show ExpY where
  showsPrec _ = showExpY

{-# INLINE showExpY #-}
showExpY (VarE x)     = showString x
showExpY (LitB True)  = showString "true"
showExpY (LitB False) = showString "false"
showExpY (LitI n) = showsPrec 0 n
showExpY (LitR r) = showsPrec 0 (numerator r) . showChar '/' . showsPrec 0 (denominator r)
showExpY (AND es) = paren (showString "and " . showListSepBy " " es)
showExpY (OR es) = paren $ showString "or " . showListSepBy " " es
showExpY (NOT e) = paren $ showString "not " . showsPrec 0 e
showExpY (e1 :=> e2) = paren $ showString "=> " . showsPrec 0 e1 . space . showsPrec 0 e2
showExpY (e1 := e2) = paren $ showString "= " . showsPrec 0 e1 . space . showsPrec 0 e2
showExpY (e1 :/= e2) = paren $ showString "/= " . showsPrec 0 e1 . space . showsPrec 0 e2
showExpY (e1 :< e2) = paren $ showString "< " . showsPrec 0 e1 . space . showsPrec 0 e2
showExpY (e1 :<= e2) = paren $ showString "<= " . showsPrec 0 e1 . space . showsPrec 0 e2
showExpY (e1 :> e2) = paren $ showString "> " . showsPrec 0 e1 . space . showsPrec 0 e2
showExpY (e1 :>= e2) = paren $ showString ">= " . showsPrec 0 e1 . space . showsPrec 0 e2
showExpY (e1 :+: e2) = paren $ showString "+ " . showsPrec 0 e1 . space . showsPrec 0 e2
showExpY (e1 :-: e2) = paren $ showString "- " . showsPrec 0 e1 . space . showsPrec 0 e2
showExpY (e1 :*: e2) = paren $ showString "* " . showsPrec 0 e1 . space . showsPrec 0 e2
showExpY (e1 :/: e2) = paren $ showString "/ " . showsPrec 0 e1 . space . showsPrec 0 e2
showExpY (DIV e1 e2) = paren $ showString "div " . showsPrec 0 e1 . space . showsPrec 0 e2
showExpY (MOD e1 e2) = paren $ showString "mod " . showsPrec 0 e1 . space . showsPrec 0 e2
showExpY (IF eb et ef) = paren $ showString "if " . showListSepBy " " [eb,et,ef]
showExpY (ITE eb et ef) = paren $ showString "ite " . showListSepBy " " [eb,et,ef]
showExpY (LET bindings e) = paren $ showString "let " . (paren $ showListSepByWith showBinding " " bindings) . space . showsPrec 0 e
  -- quantifires
showExpY (FORALL idtyps e) = paren $ showString "forall "
         . (paren $ showListSepByWith showIdTyp " " idtyps) . space . showsPrec 0 e
showExpY (EXISTS idtyps e) = paren $ showString "exists "
         . (paren $ showListSepByWith showIdTyp " " idtyps) . space . showsPrec 0 e
  -- functions
showExpY (APP e es) = paren $ showListSepBy " " (e:es)
showExpY (UPDATE_F e es v) = paren $ showString "update "
         . showsPrec 0 e . space . (paren $ showListSepBy " " es) . showsPrec 0 v
showExpY (LAMBDA idtyps e) = paren $ showString "lambda "
         . (paren $ showListSepByWith showIdTyp " " idtyps) . showsPrec 0 e
  -- tuples
showExpY (MKTUP es) = paren $ showString "mk-tuple " . showListSepBy " " es
showExpY (SELECT_T e i) = paren $ showString "select " . showsPrec 0 e . space . showsPrec 0 i
showExpY (UPDATE_T e i v) = paren $ showString "update "
         . showsPrec 0 e . space . showsPrec 0 i . space . showsPrec 0 v
  -- records
showExpY (MKREC idvals) = paren $ showString "mk-record " . showListSepByWith showIdVal " " idvals
showExpY (SELECT_R e s) = paren $ showString "select " . showsPrec 0 e . space . showString s
showExpY (UPDATE_R  e s v) = paren $ showString "update " . showsPrec 0 e . space . showString s . space . showsPrec 0 v
  -- bitvectors -- TODO

instance Show CmdY where
  showsPrec _ = showCmdY

{-# INLINE showCmdY #-}
showCmdY (DEFTYP tname Nothing) = paren $ showString "define-type " . showString tname
showCmdY (DEFTYP tname (Just t)) = paren $ showString "define-type ". showString tname . space . showsPrec 0 t
showCmdY (DEFINE idty Nothing) = paren $ showString "define " . showIdTyp idty
showCmdY (DEFINE idty (Just e)) = paren $ showString "define " . showIdTyp idty . space . showsPrec 0 e
showCmdY (ASSERT e) = paren $ showString "assert " . showsPrec 0 e
showCmdY (ASSERT_P e Nothing) = paren $ showString "assert+ " . showsPrec 0 e
showCmdY (ASSERT_P e (Just w)) = paren $ showString "assert+ " . showsPrec 0 e . space . showsPrec 0 w
showCmdY (RETRACT i) = paren $ showString "retract " . showsPrec 0 i
showCmdY (CHECK) = paren $ showString "check"
showCmdY (MAXSAT) = paren $ showString "max-sat"
showCmdY (SETE b) = paren $ showString "set-evidence! " . showsPrec 0 (LitB b)
showCmdY (SETV k) = paren $ showString "set-verbosity! " . showsPrec 0 k
showCmdY (SETAO b) = paren $ showString "set-arith-only! " . showsPrec 0 (LitB b)
showCmdY (PUSH) = paren $ showString "push"
showCmdY (POP) = paren $ showString "pop"
showCmdY (ECHO s) = paren $ showString "echo " . showsPrec 0 s -- not exact since Haskell string
showCmdY (INCLUDE s) = paren $ showString "include " . showsPrec 0 s -- not exact same reason
showCmdY (RESET) = paren $ showString "reset"
showCmdY (STATUS) = paren $ showString "status"
showCmdY (DUMP) = paren $ showString "dump-context"
showCmdY (EXIT) = paren $ showString "exit"

