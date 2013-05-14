{-| Time-stamp: <2013-02-11 23:57:37 Salman Saghafi>
  This module defines a Model structure that will be used inside a Problem structure. This module can be redefined based on the underlying implementation for models.
-}
module Chase.Problem.Model(Model(..),
                           truth,
                           empty,
                           add,
                           isTrue,
                           denotes) where

import Chase.Problem.IModel