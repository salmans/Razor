{-| 
  Razor
  Module      : Chase.HerbrandBase.RelAlg.DB
  Description : Provides an interface for interacting with the database
  Maintainer  : Salman Saghafi
-}
module Chase.HerbrandBase.RelAlg.DB (Set(..)
                                    , Select(..), Project(..)
                                    , select, project, join, fromList
                                    , emptySet 
                                    , union, difference ) where

import Chase.HerbrandBase.RelAlg.IDB
