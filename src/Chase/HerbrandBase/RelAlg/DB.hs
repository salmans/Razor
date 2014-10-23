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
                                    , empty, singleton, oneMember
                                    , IDB.null, IDB.elem, nub, size
                                    , IDB.map, IDB.filter
                                    , union, difference ) where

{- Select DB implementation: -}
-- import Chase.HerbrandBase.RelAlg.IListDB as IDB -- based on Haskell lists
import Chase.HerbrandBase.RelAlg.ISetDB as IDB -- based on Haskell sets
