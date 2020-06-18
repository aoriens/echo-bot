{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Config
import Data.IORef
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.Console
import qualified FrontEnd.Telegram
import qualified Logger
import qualified Logger.Impl

main :: IO ()
main = do
  logHandle <- getLogHandle
  botHandle <- getBotHandle logHandle
  if shouldUseTelegramFrontEnd
    then runTelegramFrontEnd logHandle botHandle
    else runConsoleFrontEnd botHandle

runConsoleFrontEnd :: EchoBot.Handle IO -> IO ()
runConsoleFrontEnd botHandle =
  FrontEnd.Console.run
    FrontEnd.Console.Handle {FrontEnd.Console.hBotHandle = botHandle}

runTelegramFrontEnd :: Logger.Handle IO -> EchoBot.Handle IO -> IO ()
runTelegramFrontEnd logHandle botHandle = do
  config <- Config.getTelegramConfig
  FrontEnd.Telegram.run
    FrontEnd.Telegram.Handle
      { FrontEnd.Telegram.hBotHandle = botHandle
      , FrontEnd.Telegram.hLogHandle = logHandle
      , FrontEnd.Telegram.hConfig = config
      }

getLogHandle :: IO (Logger.Handle IO)
getLogHandle = Logger.Impl.new <$> Config.getLoggerConfig

getBotHandle :: Logger.Handle IO -> IO (EchoBot.Handle IO)
getBotHandle logHandle = do
  botConfig <- Config.getBotConfig
  let initialState = either (error . T.unpack) id $ EchoBot.makeState botConfig
  botState <- newIORef initialState
  pure
    EchoBot.Handle
      { EchoBot.hGetState = readIORef botState
      , EchoBot.hModifyState = modifyIORef' botState
      , EchoBot.hLogHandle = logHandle
      , EchoBot.hConfig = botConfig
      }

shouldUseTelegramFrontEnd :: Bool
shouldUseTelegramFrontEnd = True
