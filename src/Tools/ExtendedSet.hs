{-| 
  Razor
  Module      : Tools.ExtendedSet
  Description : Provides extra functions to work with sets
  Maintainer  : Salman Saghafi
-}

module Tools.ExtendedSet
    ( module Set
    , mapM
    , mapM_
    , foldM
    , filterM
    , catMaybes
    , mapMaybe
    , flatten
    , concatMap
    , concatMapM
    , any
    , all
    , or
    , and
    , ss
    , toSS
    , fromSS
    , ssMapM
    , distrib
    , cartesianProduct
    , groupBy
    , partitionM
    , unzip
    ) where

import Data.Set as Set
import Prelude hiding ( mapM, mapM_, unzip, all, any, map, filter, null
                      , concatMap, and, or)
import Tools.IExtendedSet