module Datatypes where

import Data.List (intercalate)

data Sequent = Sequent [TAtom] [([VariableSymbol], [TAtom])]
instance Show Sequent where
  show (Sequent premises consequents) =
    (if null premises then "" else
       intercalate " ∧ " (map show premises) ++ " ⊢ ") ++
    if null consequents then "⊥" else intercalate " ∨ " $ do
      (vars,atoms) <- consequents
      return $
        (if null vars then "" else
           "∃ " ++ intercalate ", " (map show vars) ++ ". ") ++
        (if null atoms then "⊤" else intercalate " ∧ " $ map show atoms)

data TAtom = TPredicate PredicateSymbol [TTerm] | TEquality TTerm TTerm
instance Show TAtom where
  show (TPredicate symbol args) =
    show symbol ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (TEquality term1 term2) = show term1 ++ " = " ++ show term2

data TTerm = TVariable VariableSymbol | TFunction FunctionSymbol [TTerm]
instance Show TTerm where
  show (TVariable symbol) = show symbol
  show (TFunction symbol args) =
    (show symbol ++
     (if null args then "" else "(" ++ intercalate ", " (map show args) ++ ")"))

data MAtom = MPredicate PredicateSymbol [MTerm] | MEquality MTerm MTerm
instance Show MAtom where
  show (MPredicate symbol args) =
    show symbol ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (MEquality term1 term2) = show term1 ++ " = " ++ show term2

data MTerm = MVariable VariableSymbol | MFunction FunctionSymbol [MTerm] | ModelElement ModelElementSymbol
instance Show MTerm where
  show (MVariable symbol) = show symbol
  show (MFunction symbol args) =
    (show symbol ++
     (if null args then "" else "(" ++ intercalate ", " (map show args) ++ ")"))
  show (ModelElement symbol) = show symbol

data Model = Model [ModelElementSymbol] [Fact] deriving Show

data Fact = PredicateFact PredicateSymbol [ModelElementSymbol] | FunctionFact FunctionSymbol [ModelElementSymbol] ModelElementSymbol
instance Show Fact where
  show (PredicateFact symbol args) = 
    show symbol ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (FunctionFact symbol args value) =
    (show symbol ++
     (if null args then "" else
        "(" ++ intercalate ", " (map show args) ++ ")") ++
     " = " ++ show value)

data GraphLoc = GraphLoc [Sequent] Int [(MAtom, Int)] deriving Show

newtype VariableSymbol = VariableSymbol String
instance Show VariableSymbol where show (VariableSymbol symname) = symname

newtype PredicateSymbol = PredicateSymbol String
instance Show PredicateSymbol where show (PredicateSymbol symname) = symname

newtype FunctionSymbol = FunctionSymbol String
instance Show FunctionSymbol where show (FunctionSymbol symname) = symname

newtype ModelElementSymbol = ModelElementSymbol Int
instance Show ModelElementSymbol where
  show (ModelElementSymbol symnum) = "#" ++ show symnum

add :: GraphLoc -> MAtom -> GraphLoc
add (GraphLoc theory startingIndex steps) constraint =
  GraphLoc theory startingIndex (steps ++ [(constraint,0)])

undoConstraint :: GraphLoc -> Maybe GraphLoc
undoConstraint (GraphLoc _ _ []) = Nothing
undoConstraint (GraphLoc theory startingIndex steps) =
  Just $ GraphLoc theory startingIndex $ init steps

previousLoc :: GraphLoc -> Maybe GraphLoc
previousLoc (GraphLoc _ 0 []) = Nothing
previousLoc (GraphLoc theory startingIndex []) =
  Just $ GraphLoc theory (pred startingIndex) []
previousLoc (GraphLoc theory startingIndex steps) =
  let (constraint,index) = last steps in
  if index == 0 then Nothing else
    (Just $
     GraphLoc theory startingIndex (init steps ++ [(constraint,pred index)]))

nextLoc :: GraphLoc -> GraphLoc
nextLoc (GraphLoc theory startingIndex []) =
  GraphLoc theory (succ startingIndex) []
nextLoc (GraphLoc theory startingIndex steps) =
  let (constraint,index) = last steps in
  GraphLoc theory startingIndex (init steps ++ [(constraint,succ index)])

origin :: GraphLoc -> GraphLoc
origin (GraphLoc theory _ _) = GraphLoc theory 0 []