{-# LANGUAGE OverloadedStrings #-}

-- | A module to provide a configuration reader for other modules.
module Config
  ( getBotConfig
  ) where

import Control.Monad.Reader
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified EchoBot
import System.Environment

-- | Gets the bot config. In any case it can provide reasonable
-- default values.
getBotConfig :: IO EchoBot.Config
getBotConfig = do
  rootConf <- loadConfigFile
  runReaderT extractBotConfig $ C.subconfig "core" rootConf

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

extractBotConfig :: ReaderT C.Config IO EchoBot.Config
extractBotConfig = do
  helpReply <- lookupDefault "HelpReply" "You've entered /help command"
  repeatReply <- lookupDefault "RepeatReply" "Choose a repetition count"
  repCount <- lookupDefault "EchoRepetitionCount" 1
  pure
    EchoBot.Config
      { EchoBot.confHelpReply = helpReply
      , EchoBot.confRepeatReply = repeatReply
      , EchoBot.confRepetitionCount = repCount
      }

-- | A convenience wrapper for shortening invocations to read config
-- entries.
lookupDefault :: (C.Configured a) => C.Name -> a -> ReaderT C.Config IO a
lookupDefault key def = do
  conf <- ask
  lift $ C.lookupDefault def conf key
