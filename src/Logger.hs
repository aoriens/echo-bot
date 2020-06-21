{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}

-- | The logger interface module. It should not define a specific
-- implementation.
module Logger
  ( Logger(..)
  , Handle(..)
  , Level(..)
  , debug
  , info
  , warn
  , error
  ) where

import qualified Data.Text as T
import Prelude hiding (error, log)

-- | The actual logger implementation is hidden here.
class Logger t m
  | t -> m
  -- | Log a message
  where
  log :: t -> Level -> T.Text -> m ()

-- | A flexible implementation of Logger - a Logger class adapter.
newtype Handle m =
  Handle
    { hLog :: Level -> T.Text -> m ()
    }

instance Logger (Handle m) m where
  log = hLog

data Level
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

debug, info, warn, error :: (Logger t m) => t -> T.Text -> m ()
debug h = log h Debug

info h = log h Info

warn h = log h Warning

error h = log h Error
