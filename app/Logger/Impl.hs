{-# LANGUAGE OverloadedStrings #-}

-- | The default implementation of the Logger interface.
module Logger.Impl
  ( new
  , Handle(..)
  ) where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
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
    time <- getCurrentTime
    TIO.hPutStrLn (hFileHandle h) $ formatMessage level time text
    System.IO.hFlush (hFileHandle h)

shouldLog :: Handle -> Logger.Level -> Bool
shouldLog h level = hMinLevel h <= level

formatMessage :: Logger.Level -> UTCTime -> T.Text -> T.Text
formatMessage level time text =
  timeString <> " | " <> levelString <> " | " <> text
  where
    timeString = T.pack $ formatTime defaultTimeLocale "%F %T.%3q" time
    levelString = T.justifyLeft 7 ' ' $ T.pack (show level)
