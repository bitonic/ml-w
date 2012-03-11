module Applicative where

import Control.Monad

-- parsec 2 doeesn't have an Applicative instance...

(<$>)  :: Monad f => (a -> b) -> f a -> f b
(<$>)  = liftM

(<*>)  :: Monad m => m (a -> b) -> m a -> m b
(<*>)  = ap

(*>)   :: Monad m => m a -> m b -> m b
(*>)   = (>>)

(<*)   :: Monad m => m a -> m b -> m a
m <* n =  do x <- m; n; return x
