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

  Module      : Syntax.ITerm
  Description : The module implements the data structures for terms and its 
  building blocks.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Syntax.ITerm where

-- Standard
import Data.List
import qualified Data.Map as Map

-- import Data.Hashable

-- Control
import Control.Applicative
import qualified Control.Monad.State.Lazy as State (get, put)

-- Common
import Common.Basic

-- Tools
import Tools.Utils   (unions)
import Tools.Counter (CounterT)

-- Parsec
import Text.ParserCombinators.Parsec hiding ( (<|>) )

-- Syntax
import Syntax.FirstOrderParser


newtype Variable = Variable Sym -- Variables
    deriving (Eq, Ord)
newtype Constant = Constant Sym -- Constants
    deriving (Eq, Ord)
newtype Element  = Element Int  -- Elements of the domain
    deriving (Eq, Ord)


{- A hash function to work with IHashSet as the underlying database for the
   relational algebraic implementation of the Chase. -}
-- instance Hashable Element where
--     s `hashWithSalt` (Element e) = s `hashWithSalt` e

type FnSym  = String -- Function Symbols


unitName = "Syntax.Term"
err_invalidElementName = "invalid element name!"

{-| Term is a data-structure for first-order terms. A term is either a variable, 
 a constant, an element or a function applied to a list of terms. -}

{- It is unlikely that we benefit from indirect composite 
(http://www.haskell.org/haskellwiki/Indirect_composite) datatypes. 
Potentially, we could use this technique to separate different versions of 
terms, e.g., terms defined over elements and terms defined over constants. 

Experience from the previous version shows that it is more convenient to define
constants as a separate datatype. -}

data Term = Var  Variable
          | Cons Constant
          | Elem Element
          | Fn FnSym [Term]

          | NumberTerm Rational -- For compatibility with TPTP
          | DistinctTerm String -- For compatibility with TPTP
          deriving (Eq, Ord)

{- Provenance information in Common.Provenance are stored as skolem terms. -}
type SkolemTerm = Term

-- Looks like the default implementation (deriving) is more efficient:
-- instance Eq Term where
--     (Var  v1)       == (Var  v2)       = v1 == v2
--     (Cons c1)       == (Cons c2)       = c1 == c2
--     (Elem e1)       == (Elem e2)       = e1 == e2
--     (Fn f1 ts1)     == (Fn f2 ts2)     = f1 == f2 && and (zipWith (==) ts1 ts2)
--     NumberTerm r1   == NumberTerm r2   = r1 == r2
--     DistinctTerm t1 == DistinctTerm t2 = t1 == t2
--     t1 == t2 = False -- otherwise

{- Show instances -}
instance Show Element where
    show (Element e) = "e^" ++ show e

instance Show Variable where
    show (Variable v) = v

instance Show Constant where
    show (Constant c) = "'" ++ c

instance Show Term where
    show t = prettyTerm t


{-| Reads an element from a String. This function maintains the following 
  invariant: @readElement (show e) = e@ -}
readElement :: String -> Element
readElement ('e':'^':e) = Element (read e)
readElement e           = error $ unitName ++ ".readElement: " 
                          ++ err_invalidElementName

{-| A substitution from a 'Variable' to a 'Term'. -}
type Sub = Map.Map Variable Term

{-| Empty Substitution -}
emptySub :: Sub
emptySub =  Map.empty

{-| A constant substitution from a 'Constant' to a 'Term'. -}
type ConsSub = Map.Map Constant Term

{-| Empty Constant Substitution -}
emptyConsSub :: ConsSub
emptyConsSub =  Map.empty

{-| An existential substitution is a substitution that is applied on 
  existentially  quantified variables. However, since existentially quantified 
  variables are not unique at the level of formulas (also sequents), they are
  uniquely identified by their Skolem functions, assuming that Skolem functions 
  are unique. Therefore, an existential substitution is a map from Skolem 
  functions to terms. -}
type ExistsSub = Map.Map FnSym Term

{-| Empty Existential Substitution -}
emptyExistsSub = Map.empty
--------------------------------------------------------------------------------
-- Pretty Printing

prettyTerm :: Term -> String
prettyTerm (Var v)   = show v
prettyTerm (Cons c)  = show c
prettyTerm (Elem e)  = show e
prettyTerm (Fn f ts) = f ++ "(" ++ (intercalate "," (prettyTerm <$> ts)) ++ ")"


{- parseTerm parses a string as a term. 

Parameters:
[@input@] string to parse
-}
parseTerm :: String -> Term
parseTerm str =
 let pResult =  parse pTerm "parsing Term" str
    in case pResult of
           Left err -> error (show err)
           Right val -> val

{- Parses terms over the extended language -}
xparseTerm :: String -> Term
xparseTerm str =
 let pResult =  parse xpTerm "parsing Term" str
    in case pResult of
           Left err -> error (show err)
           Right val -> val

{- Parses the identifier and then lets the next thing decide whether its a
   variable name or a function name (depending on whether there is an argument
   list) -}
pTerm :: Parser Term
pTerm = pConstant <|> (identifier >>= pTermWithIden)
    <?> "term"

{- parsing 'Element's -}
pElement :: Parser Term
pElement = do
  symbol "e^" -- for now
  number <- natural
  return $ Elem $ Element (fromIntegral number)

{- Extends pTerm, allows for parsing elements -}
xpTerm :: Parser Term
xpTerm = pElement <|> pConstant <|> (identifier >>= xpTermWithIden)
         <?> "term"

{- Parses a constant -}
pConstant :: Parser Term
pConstant = do
  symbol "'"
  elem   <- identifier
  return $ Cons $ Constant elem

{- Parser for terms with leading identifier already parsed. This is the "next
   thing" that decides whether the given string is a variable name or a function
   name. -}
pTermWithIden :: String -> Parser Term
pTermWithIden name
    = Fn name <$> pTermList
    <|> return (Var (Variable name))

{- Like pTermWithIden but works on the extended language. -}
xpTermWithIden :: String -> Parser Term
xpTermWithIden name
    = Fn name <$> xpTermList
    <|> return (Var (Variable name))

{- A list of terms separated by commas. -}
pTermList :: Parser [Term]
pTermList = (parens $ commaSep pTerm)
         <?> "list of terms"

{- Like pTermList but works on the extended language. -}
xpTermList :: Parser [Term]
xpTermList = (parens $ commaSep xpTerm)
         <?> "list of terms"
--------------------------------------------------------------------------------
{-| TermBased is the class of types that are formed by 'Term's or have 'Term's 
  as a part of their definition, e.g. 'Formula' and 'Sequent':
  [@freeVars@] returns a list of free variables in an instance of type @a@.
  [@functionSyms@] returns a list of pairs, containing function symbols and 
  their arities in an instance of type @a@. 
  [@substitute@] applies a 'Sub' to a 'TermBased' instance.
-}
class TermBased a where
    freeVars            :: a -> [Variable]
    constants           :: a -> [Constant]
    functionSyms        :: a -> [(FnSym, Int)]
    substitute          :: Sub -> a -> a
    substituteConstants :: ConsSub -> a -> a

instance TermBased a => TermBased [a] where
    freeVars                = nub.(concatMap freeVars)
    constants               = nub.(concatMap constants)
    functionSyms            = nub.(concatMap functionSyms)
    substitute env          = (substitute env <$>)
    substituteConstants env = (substituteConstants env <$>)

{- Term is TermBased -}
instance TermBased Term where
    freeVars (Var x)    = [x]
    freeVars (Fn _ ts)  = unions (freeVars <$> ts)
    freeVars (Elem _)   = []
    freeVars (Cons _)   = []

    constants (Var _)   = []
    constants (Fn c []) = [Constant c]
    constants (Fn _ ts) = unions (constants <$> ts)
    constants (Elem _)  = []
    constants (Cons c)  = [c]

    functionSyms (Var _)     = []
    functionSyms (Cons (Constant c))    
                             = [(c, 0)] -- constants as nullary functions
    functionSyms (Fn f args) = foldr (union . functionSyms) 
                               [(f, length args)] args 

    substitute env t@(Var v@(Variable _)) = case Map.lookup v env of
                                              Just t' -> t'
                                              Nothing -> t
    substitute env t@(Elem _)             = t
    substitute env t@(Cons (Constant _))  = t
    substitute env t@(Fn f ts)            = Fn f $ (substitute env) <$> ts

    substituteConstants env t@(Var _)   = t
    substituteConstants env t@(Elem _)  = t
    substituteConstants env t@(Cons c)  = case Map.lookup c env of
                                            Just t' -> t'
                                            Nothing -> t                                            
    substituteConstants env t@(Fn c []) = case Map.lookup (Constant c) env of
                                            Just t' -> t'
                                            Nothing -> t
    substituteConstants env t@(Fn f ts) = Fn f $ (substituteConstants env) <$> ts
--------------------------------------------------------------------------------
-- Functions to work with Terms

{-| Retunrs a fresh 'Variable' in form @v#i@ where @i@ is an index number.
  The function assigns an index to the variable in a 'Counter' monad defined 
  in "Tools.Counter". -}
freshVariable :: Monad m => CounterT m Variable
freshVariable = State.get >>= 
                (\c -> State.put (c + 1) >> 
                (return $ Variable $ "v#" ++ (show c)))

{-| Retunrs a fresh 'Constant' in form @c#i@ where @i@ is an index number. 
  The function assigns an index to the constant in a 'Counter' monad defined 
  in "Tools.Counter". -}
freshConstant :: Monad m => CounterT m Constant
freshConstant = State.get >>= 
                (\c -> State.put (c + 1) >> 
                (return $ Constant $ "c#" ++ (show c)))

{-| Retunrs a fresh 'Element' in form @e#i@ where @i@ is an index number.
  The function assigns an index to the element in a 'Counter' monad defined 
  in "Tools.Counter". -}
freshElement :: Monad m => CounterT m Element
freshElement = State.get >>= 
               (\c -> State.put (c + 1) >> 
               (return $ Element c))

{-| Returns True if an input term is a 'Variable', otherwise returns False. -}
isVariable :: Term -> Bool
isVariable (Var _) = True
isVariable _       = False

{-| Returns True if an input term is an 'Element', otherwise returns False. -}
isElement :: Term -> Bool
isElement (Elem _) = True
isElement _        = False

{-| Returns True if an input term is a 'Constant', otherwise returns False. -}
isConstant :: Term -> Bool
isConstant (Cons _) = True
isConstant _        = False


{-| For an input 'Term' @t@, returns its enclosed 'Variable', if @t@ is a single
  variable term, inside Maybe. -}
termToVariable :: Term -> Maybe Variable
termToVariable (Var v) = Just v
termToVariable _       = Nothing

{-| For an input 'Term' @t@, returns its enclosed 'Constant', if @t@ is a single
  constant term, inside Maybe. Since we allow nullary functions to act like 
  constants, the function also converts nullary function terms to an equivalent 
  'Constant'. -}
termToConstant :: Term -> Maybe Constant
termToConstant (Cons c)  = Just c
termToConstant (Fn f []) = Just $ Constant f 
termToConstant _         = Nothing

{-| For an input 'Term' @t@, returns its enclosed 'Element' if @t@ is a single
  element term, inside Maybe. -}
termToElement :: Term -> Maybe Element
termToElement (Elem e) = Just e
termToElement _        = Nothing

{-| Returns the depth of the term (starting with 0 for constants, variables and
  elements. -}
termDepth (Var _)   = 0
termDepth (Fn _ []) = 0
termDepth (Elem _)  = 0
termDepth (Cons _)  = 0
termDepth (Fn _ ts) = 1 + maximum (termDepth <$> ts)

{-| Renames the input variable so that it does not exist in the given list of
  variables. -}
variant :: Variable -> [Variable] -> Variable
variant v@(Variable var) vs = if v `elem` vs 
                              then variant (Variable (var ++ "'")) vs
                              else v
