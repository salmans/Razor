{- Razor
   Module      : UI.Ansi.IDisplay
   Description : Internal implementation for display.
   Maintainer  : Salman Saghafi -}

module UI.Ansi.IDisplay where

-- Standard
import Data.List
import Control.Applicative
-- Syntax
import Syntax.Term



instance Show Element where
    show (Element e) = "[" ++ e ++ "]"

instance Show Variable where
    show (Variable v) = v

instance Show Constant where
    show (Constant c) = c

instance Show Term where
    show t = prettyTerm t

prettyTerm :: Term -> String
prettyTerm (Var v)   = show v
prettyTerm (Cons c)  = show c
prettyTerm (Elem e)  = show e
prettyTerm (Fn f ts) = f ++ "(" ++ (intercalate "," (prettyTerm <$> ts)) ++ ")"