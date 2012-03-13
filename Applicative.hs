{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | This module exists only because parsec 2 doesn't have an
--   'Applicative' instance, and in general because it's annoying to add
--   'Applicative' to all the constraints.
module Applicative where

import Control.Monad

-- parsec 2 doeesn't have an Applicative instance...

(<$>)  :: Monad m => (a -> b) -> m a -> m b
(<$>)  = liftM

(<*>)  :: Monad m => m (a -> b) -> m a -> m b
(<*>)  = ap

(*>)   :: Monad m => m a -> m b -> m b
(*>)   = (>>)

(<*)   :: Monad m => m a -> m b -> m a
m <* n = do x <- m; n; return x
