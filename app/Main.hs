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
  botState <- newIORef EchoBot.makeState
  pure
    EchoBot.Handle
      { EchoBot.hGetState = readIORef botState
      , EchoBot.hModifyState = modifyIORef' botState
      , EchoBot.hLogHandle = logHandle
      }
