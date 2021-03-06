{-# LANGUAGE OverloadedStrings, FlexibleInstances, LambdaCase #-}

-- | A module to provide a configuration reader for other modules.
module Config
  ( getBotConfig
  , getLoggerConfig
  , getTelegramConfig
  , getFrontEndConfig
  ) where

import Control.Exception
import Control.Monad.Reader
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.Telegram
import qualified FrontEnd.Telegram.Impl.HTTP as TelegramHttp
import qualified Logger
import qualified Logger.Impl
import qualified Main.ConfigurationTypes as Main
import System.Environment
import System.IO

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
    level <- unwrap <$> lookupDefault "Verbosity" (Wrapper Logger.Info)
    path <- lookupDefault "Path" ""
    fileHandle <- lift $ openLogFile path
    pure
      Logger.Impl.Handle
        {Logger.Impl.hFileHandle = fileHandle, Logger.Impl.hMinLevel = level}

getTelegramConfig :: IO (FrontEnd.Telegram.Config, TelegramHttp.Config)
getTelegramConfig =
  withConfigFileSection "telegram" $ do
    apiToken <- require "ApiToken"
    urlPrefix <- lookupDefault "URLPrefix" "https://api.telegram.org"
    pollTimeout <- lookupDefault "PollTimeout" 60
    connectionTimeout <- lookupDefault "ConnectionTimeout" 30
    pure
      ( FrontEnd.Telegram.Config
          { FrontEnd.Telegram.confApiToken = apiToken
          , FrontEnd.Telegram.confURLPrefixWithoutTrailingSlash =
              T.unpack $ unslash urlPrefix
          , FrontEnd.Telegram.confPollTimeout = pollTimeout
          , FrontEnd.Telegram.confOutdatedMenuTitleSuffix =
              "The menu is now out-of-date"
          }
      , TelegramHttp.Config
          {TelegramHttp.confResponseTimeout = connectionTimeout})
  where
    unslash = T.dropWhileEnd (== '/')

getFrontEndConfig :: IO Main.FrontEnd
getFrontEndConfig =
  withConfigFileSection "core" $
  unwrap <$> lookupDefault "FrontEnd" (Wrapper Main.ConsoleFrontEnd)

openLogFile :: FilePath -> IO Handle
openLogFile "" = pure stderr
openLogFile path =
  try (openFile path AppendMode) >>= \case
    Right h -> pure h
    Left e -> do
      let _ = (e :: IOException)
      hPutStrLn stderr $
        "Error while opening log, falling back to stderr: " ++ show e
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

lookupDefault :: (C.Configured a) => C.Name -> a -> ReaderT C.Config IO a
lookupDefault key def = do
  conf <- ask
  lift $ C.lookupDefault def conf key

require :: (C.Configured a) => C.Name -> ReaderT C.Config IO a
require key =
  lookupDefault
    key
    (error $ "'" ++ T.unpack key ++ "' must be specified in configuration")

-- | A wrapper type to avoid orphan instance warning
newtype Wrapper a =
  Wrapper
    { unwrap :: a
    }

instance C.Configured (Wrapper Logger.Level) where
  convert (C.String "debug") = Just $ Wrapper Logger.Debug
  convert (C.String "info") = Just $ Wrapper Logger.Info
  convert (C.String "warning") = Just $ Wrapper Logger.Warning
  convert (C.String "error") = Just $ Wrapper Logger.Error
  convert _ = Nothing

instance C.Configured (Wrapper Main.FrontEnd) where
  convert (C.String "console") = Just $ Wrapper Main.ConsoleFrontEnd
  convert (C.String "telegram") = Just $ Wrapper Main.TelegramFrontEnd
  convert _ = Nothing
