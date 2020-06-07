-- | The logger interface module. It should not define a specific
-- implementation.
module Logger
  ( Handle(..)
  , Level(..)
  , debug
  , info
  , warn
  , error
  ) where

import qualified Data.Text as T
import Prelude hiding (error, log)

-- | The actual logger implementation is hidden here.
newtype Handle m =
  Handle
    { log :: Level -> T.Text -> m ()
    }

data Level
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

debug, info, warn, error :: (Monad m) => Handle m -> T.Text -> m ()
debug h = log h Debug

info h = log h Info

warn h = log h Warning

error h = log h Error
