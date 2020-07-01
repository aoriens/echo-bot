{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

-- | The module introduces the flexible state type class and handle.
module Util.FlexibleState
  ( Class(..)
  , Handle(..)
  ) where

-- | An extended analogue of MonadState which is parameterized with
-- additional handle type. It easily allows to have multiple
-- implementations of state handling in a single monad type.
class Monad m =>
      Class h s m
  | h -> s
  , h -> m
  where
  {-# MINIMAL get, (modify' | put) #-}
  get :: h -> m s
  put :: h -> s -> m ()
  modify' :: h -> (s -> s) -> m ()
  put h = modify' h . const
  modify' h f = get h >>= \x -> put h $! f x

-- | A flexible adapter instance of FlexibleState class - the handle
-- type.
data Handle s m =
  Handle
    { hGet :: m s
    , hModify' :: (s -> s) -> m ()
    }

instance Monad m => Class (Handle s m) s m where
  get = hGet
  modify' = hModify'
