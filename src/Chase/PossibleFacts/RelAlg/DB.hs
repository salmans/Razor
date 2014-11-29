{-| 
  Razor
  Module      : Chase.PossibleFacts.RelAlg.DB
  Description : Provides an interface for interacting with the database
  Maintainer  : Salman Saghafi
-}
module Chase.PossibleFacts.RelAlg.DB ( Set(..)
                                    , Select(..), Project(..)
                                    , select, project, join
                                    , fromList, toList
                                    , empty, singleton, oneMember, insert
                                    , IDB.null, IDB.elem, nub, size
                                    , IDB.map, IDB.filter
                                    , union, unions, difference ) where

{- Select DB implementation: -}
-- import Chase.PossibleFacts.RelAlg.IListDB as IDB -- based on Haskell lists
import Chase.PossibleFacts.RelAlg.ISetDB as IDB -- based on Data.Set
-- import Chase.PossibleFacts.RelAlg.IHashSetDB as IDB -- based on Data.HashSet
