{- Razor
   Module      : Tools.ICounter
   Description : The module implements an interface for working with Counter,
   which is used to keep track of a counting value in a computation.
   Maintainer  : Salman Saghafi -}

module Tools.ICounter where

-- Control
import qualified Control.Monad.State.Lazy as State

{-| Monad and monad transformer for counting. -}
type Counter  = State.State Int
type CounterT = State.StateT Int


{-| Increments the value in a 'Counter' and returns the current value of the 
  counter. -}
increment :: Counter Int
increment =  State.get >>= 
             (\c -> State.modify (+1) >>
             (return c))

{-| Increments the value in a 'CounterT' and returns the current value of the
  counter. -}
incrementT :: Monad m => CounterT m Int
incrementT = State.get >>= 
             (\c -> State.modify (+1) >>
             (return c))