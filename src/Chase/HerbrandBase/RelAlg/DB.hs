{-| 
  Razor
  Module      : Chase.HerbrandBase.RelAlg.DB
  Description : Provides an interface for interacting with the database
  Maintainer  : Salman Saghafi
-}
module Chase.HerbrandBase.RelAlg.DB ( Set(..)
                                    , Select(..), Project(..)
                                    , select, project, join
                                    , fromList, toList
                                    , empty, singleton, oneMember, insert
                                    , IDB.null, IDB.elem, nub, size
                                    , IDB.map, IDB.filter
                                    , union, unions, difference ) where

{- Select DB implementation: -}
-- import Chase.HerbrandBase.RelAlg.IListDB as IDB -- based on Haskell lists
-- import Chase.HerbrandBase.RelAlg.ISetDB as IDB -- based on Data.Set
import Chase.HerbrandBase.RelAlg.IHashSetDB as IDB -- based on Data.HashSet
