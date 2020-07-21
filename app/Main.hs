{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Config
import Control.Monad
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.Console
import qualified FrontEnd.Telegram as Telegram
import qualified FrontEnd.Telegram.Impl.HTTP
import qualified Logger
import qualified Logger.Impl
import Main.ConfigurationTypes
import qualified Util.FlexibleState as FlexibleState

main :: IO ()
main = do
  logHandle <- getLogHandle
  frontEnd <- Config.getFrontEndConfig
  case frontEnd of
    TelegramFrontEnd -> runTelegramFrontEnd logHandle (getBotHandle logHandle)
    ConsoleFrontEnd -> runConsoleFrontEnd =<< getBotHandle logHandle

runConsoleFrontEnd :: EchoBot.Handle IO -> IO ()
runConsoleFrontEnd botHandle =
  FrontEnd.Console.run
    FrontEnd.Console.Handle {FrontEnd.Console.hBotHandle = botHandle}

runTelegramFrontEnd :: Logger.Handle IO -> IO (EchoBot.Handle IO) -> IO ()
runTelegramFrontEnd logHandle makeBotHandle = do
  (teleConfig, implConfig) <- Config.getTelegramConfig
  telegramHandle <-
    FrontEnd.Telegram.Impl.HTTP.new logHandle implConfig teleConfig
  go telegramHandle IntMap.empty
  where
    go telegramHandle botHandles = do
      events <- Telegram.receiveEvents telegramHandle
      botHandles' <- foldM (handleEvent telegramHandle) botHandles events
      go telegramHandle botHandles'
    handleEvent telegramHandle botHandles (chatId, event) = do
      (botHandle, botHandles') <- getBotHandleForChat botHandles chatId
      r <- EchoBot.respond botHandle event
      Telegram.handleBotResponse telegramHandle chatId r
      pure botHandles'
    getBotHandleForChat botHandles (Telegram.ChatId chatId) =
      case IntMap.lookup chatId botHandles of
        Just h -> pure (h, botHandles)
        Nothing -> do
          h <- makeBotHandle
          pure (h, IntMap.insert chatId h botHandles)

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
