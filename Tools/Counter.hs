module Tools.Counter where

import Control.Monad.State.Lazy as State
import Formula.SyntaxGeo

-- Salman: Move it somewhere appropriate
type Counter  = State.State Int
type CounterT = State.StateT Int

increment :: Counter Int
increment =  State.get >>= 
             (\c -> State.put (c + 1) >> (return $ c))

incrementT :: Monad m => CounterT m Int
incrementT = State.get >>= 
             (\c -> State.put (c + 1) >> (return $ c))

freshVar :: Counter Var
freshVar = State.get >>= 
           (\c -> State.put (c + 1) >> 
           (return $ "x#" ++ (show c)))

freshElement :: Counter Elem
freshElement = get >>=
               (\c -> State.put (c + 1) >>
               (return $ Elem $ "e#" ++ (show c)))

freshSymbol :: Sym -> Counter Sym
freshSymbol sym = State.get >>=
                  (\c -> State.put (c + 1) >>
                  (return $ sym ++ "#" ++ (show c)))