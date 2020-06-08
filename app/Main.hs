{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Data.IORef
import qualified EchoBot
import qualified FrontEnd.Console
import qualified Logger
import qualified Logger.Impl
import System.IO

main :: IO ()
main = do
  botHandle <- getBotHandle
  let consoleFrontEndHandle =
        FrontEnd.Console.Handle {FrontEnd.Console.hBotHandle = botHandle}
  FrontEnd.Console.run consoleFrontEndHandle

logHandle :: Logger.Handle IO
logHandle =
  Logger.Impl.new
    Logger.Impl.Handle
      {Logger.Impl.hFileHandle = stderr, Logger.Impl.hMinLevel = Logger.Info}

getBotHandle :: IO (EchoBot.Handle IO)
getBotHandle = do
  botState <- newIORef $ EchoBot.makeState botConfig
  pure
    EchoBot.Handle
      { EchoBot.hGetState = readIORef botState
      , EchoBot.hModifyState = modifyIORef' botState
      , EchoBot.hLogHandle = logHandle
      , EchoBot.hConfig = botConfig
      }

botConfig :: EchoBot.Config
botConfig =
  EchoBot.Config
    { EchoBot.confHelpReply = "This is usage description"
    , EchoBot.confRepeatReply =
        "The current repetition amount is {count}.\nSelect the number of repetitions"
    , EchoBot.confRepetitionCount = 1
    }
