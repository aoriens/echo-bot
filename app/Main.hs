{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Data.IORef
import qualified Data.Text.IO as TIO
import qualified EchoBot
import qualified FrontEnd.Console
import qualified Logger

main :: IO ()
main = do
  let logHandle = Logger.Handle {Logger.log = \_ text -> TIO.putStrLn text}
  botHandle <- getBotHandle logHandle
  let consoleFrontEndHandle =
        FrontEnd.Console.Handle {FrontEnd.Console.hBotHandle = botHandle}
  FrontEnd.Console.run consoleFrontEndHandle

getBotHandle :: Logger.Handle IO -> IO (EchoBot.Handle IO)
getBotHandle logHandle = do
  botState <- newIORef EchoBot.makeState
  pure
    EchoBot.Handle
      { EchoBot.hGetState = readIORef botState
      , EchoBot.hModifyState = modifyIORef' botState
      , EchoBot.hLogHandle = logHandle
      }
