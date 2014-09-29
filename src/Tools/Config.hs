{-| 
  Razor
  Module      : Tools.Config
  Description : Offers datatype and functions to for managing user preferences.
  Maintainer  : Salman Saghafi -}

module Tools.Config ( InputType (..), Config (..)
                    , ConfigMonad, ConfigMonadT
                    , defaultConfig ) where

import Tools.IConfig