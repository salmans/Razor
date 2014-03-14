module Tools.Counter where

import Control.Monad.State.Lazy as State
import Formula.SyntaxGeo

-- Salman: Move it somewhere appropriate
type Counter  = State.State Int
type CounterT = State.StateT Int

lift :: (Monad m, MonadTrans t) => m a  -> t m a
lift = State.lift

increment :: Counter Int
increment =  State.gets (+1)

incrementT :: Monad m => CounterT m Int
incrementT = State.gets (+1)

freshVar :: Counter Var
freshVar = State.get >>= 
           (\c -> State.put (c + 1) >> 
           (return $ "x#" ++ (show c)))

freshElement :: Counter Elem
freshElement = get >>=
               (\c -> State.put (c + 1) >>
               (return $ Elem $ "e#" ++ (show c)))

freshElementT :: Monad m => CounterT m Elem
freshElementT = get >>=
                (\c -> State.put (c + 1) >>
                 (return $ Elem $ "e#" ++ (show c)))

freshSymbol :: Sym -> Counter Sym
freshSymbol sym = State.get >>=
                  (\c -> State.put (c + 1) >>
                  (return $ sym ++ "#" ++ (show c)))