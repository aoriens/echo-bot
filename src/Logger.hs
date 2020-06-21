{-# LANGUAGE FlexibleInstances, FunctionalDependencies, RankNTypes
  #-}

-- | The logger interface module. It should not define a specific
-- implementation.
module Logger
  ( Logger(..)
  , Handle(..)
  , Level(..)
  , CallSite(..)
  , debug
  , info
  , warn
  , error
  , log
  , (.<)
  ) where

import Data.Maybe
import qualified Data.Text as T
import GHC.Stack
import Prelude hiding (error, log)

-- | The actual logger implementation is hidden here.
class Logger t m
  | t -> m
  -- | Log a message
  where
  lowLevelLog :: t -> Level -> Maybe CallSite -> T.Text -> m ()

-- | A flexible implementation of Logger - a Logger class adapter.
newtype Handle m =
  Handle
    { hLowLevelLog :: Level -> Maybe CallSite -> T.Text -> m ()
    }

instance Logger (Handle m) m where
  lowLevelLog = hLowLevelLog

data Level
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

debug, info, warn, error :: (HasCallStack, Logger t m) => t -> T.Text -> m ()
debug h = lowLevelLog h Debug (captureTopCallSite callStack)

info h = lowLevelLog h Info (captureTopCallSite callStack)

warn h = lowLevelLog h Warning (captureTopCallSite callStack)

error h = lowLevelLog h Error (captureTopCallSite callStack)

log :: (HasCallStack, Logger t m) => t -> Level -> T.Text -> m ()
log h level = lowLevelLog h level (captureTopCallSite callStack)

data CallSite =
  CallSite
    { csModule :: T.Text
    , csStartLine :: Int
    }

captureTopCallSite :: CallStack -> Maybe CallSite
captureTopCallSite = fmap makeCallSite . listToMaybe . getCallStack
  where
    makeCallSite (_, loc) =
      CallSite
        { csModule = T.pack $ srcLocModule loc
        , csStartLine = srcLocStartLine loc
        }

-- | Concatenate a text and an instance of Show. This is a convenience
-- function to shorten invocations of logger functions.
(.<) :: (Show a) => T.Text -> a -> T.Text
text .< a = text <> T.pack (show a)

infixr 7 .<
