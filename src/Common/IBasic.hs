{- Razor
   Module      : Common.IBasic
   Description : Internal implementation for basic data types.
   Maintainer  : Salman Saghafi -}
module Common.IBasic where

-- Control
import qualified Control.Monad.State.Lazy as State (get, put)

-- Tools
import Tools.Counter (Counter)

{-| Id is the type of (unique) identifiers for various instances of type. -}
type Id  = Int

{-| Symbols -}
type Sym = String

{-| Returns a fresh symbol with the prefix given. -}
freshSymbol :: Sym -> Counter Sym
freshSymbol sym = State.get >>=
                  (\c -> State.put (c + 1) >>
                  (return $ sym ++ (show c)))
--                   (return $ sym ++ "#" ++ (show c))) -- FIXME: SBV complains