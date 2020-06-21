{-# LANGUAGE OverloadedStrings #-}

-- | The default implementation of the Logger interface.
module Logger.Impl
  ( new
  , Handle(..)
  ) where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Logger
import Prelude hiding (log)
import qualified System.IO

data Handle =
  Handle
    { hFileHandle :: System.IO.Handle
    , hMinLevel :: Logger.Level
    }

new :: Handle -> Logger.Handle IO
new h = Logger.Handle {Logger.hLog = log h}

log :: Handle -> Logger.Level -> T.Text -> IO ()
log h level text = do
  when (shouldLog h level) $ do
    TIO.hPutStrLn (hFileHandle h) $ formatMessage h level text
    System.IO.hFlush (hFileHandle h)

shouldLog :: Handle -> Logger.Level -> Bool
shouldLog h level = hMinLevel h <= level

formatMessage :: Handle -> Logger.Level -> T.Text -> T.Text
formatMessage _ level text =
  T.justifyLeft 10 ' ' (T.pack (show level) <> ":") <> " " <> text
