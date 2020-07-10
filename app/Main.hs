{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Config
import Control.Monad
import Data.IORef
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.Console
import qualified FrontEnd.Telegram
import qualified FrontEnd.Telegram.Impl.HTTP
import qualified Logger
import qualified Logger.Impl
import Main.ConfigurationTypes
import qualified Util.FlexibleState as FlexibleState

main :: IO ()
main = do
  logHandle <- getLogHandle
  botHandle <- getBotHandle logHandle
  frontEnd <- Config.getFrontEndConfig
  case frontEnd of
    TelegramFrontEnd -> runTelegramFrontEnd logHandle botHandle
    ConsoleFrontEnd -> runConsoleFrontEnd botHandle

runConsoleFrontEnd :: EchoBot.Handle IO -> IO ()
runConsoleFrontEnd botHandle =
  FrontEnd.Console.run
    FrontEnd.Console.Handle {FrontEnd.Console.hBotHandle = botHandle}

runTelegramFrontEnd :: Logger.Handle IO -> EchoBot.Handle IO -> IO ()
runTelegramFrontEnd logHandle botHandle = do
  (teleConfig, implConfig) <- Config.getTelegramConfig
  handle <- FrontEnd.Telegram.Impl.HTTP.new logHandle implConfig teleConfig
  forever $ do
    events <- FrontEnd.Telegram.receiveEvents handle
    forM_ events $ \(chatId, event) ->
      FrontEnd.Telegram.handleResponse handle chatId =<<
      EchoBot.respond botHandle event

getLogHandle :: IO (Logger.Handle IO)
getLogHandle = Logger.Impl.new <$> Config.getLoggerConfig

getBotHandle :: Logger.Handle IO -> IO (EchoBot.Handle IO)
getBotHandle logHandle = do
  botConfig <- Config.getBotConfig
  let initialState = either (error . T.unpack) id $ EchoBot.makeState botConfig
  botState <- newIORef initialState
  pure
    EchoBot.Handle
      { EchoBot.hStateHandle =
          FlexibleState.Handle
            { FlexibleState.hGet = readIORef botState
            , FlexibleState.hModify' = modifyIORef' botState
            }
      , EchoBot.hLogHandle = logHandle
      , EchoBot.hConfig = botConfig
      }
