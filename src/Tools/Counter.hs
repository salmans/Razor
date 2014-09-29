{-| 
  Razor
  Module      : Tools.Counter
  Description : Provides an interface to work with 'Counter' monad and 
  'CounterT' monad transformer.
  Maintainer  : Salman Saghafi -}

module Tools.Counter ( Counter, CounterT
                     , increment, incrementT) where

import Tools.ICounter