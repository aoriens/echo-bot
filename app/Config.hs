{-# LANGUAGE OverloadedStrings #-}

-- | A module to provide a configuration reader for other modules.
module Config
  ( getBotConfig
  , getLoggerConfig
  ) where

import Control.Monad.Reader
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified EchoBot
import qualified Logger
import qualified Logger.Impl
import System.Environment
import System.IO
import System.IO.Error

-- | Gets the bot config. In any case it can provide reasonable
-- default values.
getBotConfig :: IO EchoBot.Config
getBotConfig =
  withConfigFileSection "core" $ do
    helpReply <- lookupDefault "HelpReply" "You've entered /help command"
    repeatReply <- lookupDefault "RepeatReply" "Choose a repetition count"
    repCount <- lookupDefault "EchoRepetitionCount" 1
    pure
      EchoBot.Config
        { EchoBot.confHelpReply = helpReply
        , EchoBot.confRepeatReply = repeatReply
        , EchoBot.confRepetitionCount = repCount
        }

getLoggerConfig :: IO Logger.Impl.Handle
getLoggerConfig =
  withConfigFileSection "log" $ do
    (LogLevelWrapper level) <-
      lookupDefault "Verbosity" $ LogLevelWrapper Logger.Info
    path <- lookupDefault "Path" ""
    fileHandle <- lift $ openLogFile path
    pure
      Logger.Impl.Handle
        {Logger.Impl.hFileHandle = fileHandle, Logger.Impl.hMinLevel = level}

openLogFile :: FilePath -> IO Handle
openLogFile "" = pure stderr
openLogFile path = do
  openFile path AppendMode `catchIOError` \e -> do
    hPutStrLn stderr $ "Error while opening log: " ++ show e
    pure stderr

withConfigFileSection :: C.Name -> ReaderT C.Config IO a -> IO a
withConfigFileSection section m = do
  rootConf <- loadConfigFile
  runReaderT m $ C.subconfig section rootConf

loadConfigFile :: IO C.Config
loadConfigFile = do
  paths <- getConfigPaths
  C.load $ map C.Optional paths

getConfigPaths :: IO [FilePath]
getConfigPaths = do
  args <- getArgs
  case args of
    ("--config":path:_) -> pure [path]
    _ -> pure []

-- | A convenience wrapper for shortening invocations to read config
-- entries.
lookupDefault :: (C.Configured a) => C.Name -> a -> ReaderT C.Config IO a
lookupDefault key def = do
  conf <- ask
  lift $ C.lookupDefault def conf key

-- | A wrapper type to avoid orphan instance warning
newtype LogLevelWrapper =
  LogLevelWrapper Logger.Level

instance C.Configured LogLevelWrapper where
  convert (C.String "debug") = Just $ LogLevelWrapper Logger.Debug
  convert (C.String "info") = Just $ LogLevelWrapper Logger.Info
  convert (C.String "warning") = Just $ LogLevelWrapper Logger.Warning
  convert (C.String "error") = Just $ LogLevelWrapper Logger.Error
  convert _ = Nothing
