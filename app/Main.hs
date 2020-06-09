{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Config
import Data.IORef
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.Console
import qualified Logger
import qualified Logger.Impl

main :: IO ()
main = do
  botHandle <- getBotHandle
  let consoleFrontEndHandle =
        FrontEnd.Console.Handle {FrontEnd.Console.hBotHandle = botHandle}
  FrontEnd.Console.run consoleFrontEndHandle

getLogHandle :: IO (Logger.Handle IO)
getLogHandle = Logger.Impl.new <$> Config.getLoggerConfig

getBotHandle :: IO (EchoBot.Handle IO)
getBotHandle = do
  botConfig <- Config.getBotConfig
  let initialState = either (error . T.unpack) id $ EchoBot.makeState botConfig
  botState <- newIORef initialState
  logHandle <- getLogHandle
  pure
    EchoBot.Handle
      { EchoBot.hGetState = readIORef botState
      , EchoBot.hModifyState = modifyIORef' botState
      , EchoBot.hLogHandle = logHandle
      , EchoBot.hConfig = botConfig
      }
