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
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
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
  httpManager <- HTTP.newManager TLS.tlsManagerSettings
  telegramHandle <-
    FrontEnd.Telegram.Impl.HTTP.new httpManager logHandle implConfig teleConfig
  go telegramHandle IntMap.empty
  where
    go telegramHandle botHandles = do
      events <- Telegram.receiveEvents telegramHandle
      botHandles' <- foldM (handleEvent telegramHandle) botHandles events
      go telegramHandle botHandles'
    handleEvent telegramHandle botHandles (chatId, event) = do
      (botHandle, botHandles') <- getBotHandleForChat botHandles chatId
      EchoBot.respond botHandle event >>=
        mapM_ (Telegram.handleBotResponse telegramHandle chatId)
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
