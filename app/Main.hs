{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Data.IORef
import qualified EchoBot
import qualified FrontEnd.Console

main :: IO ()
main = do
  botState <- newIORef EchoBot.makeState
  FrontEnd.Console.run $
    FrontEnd.Console.Handle
      { FrontEnd.Console.hBotHandle =
          EchoBot.Handle
            { EchoBot.hGetState = readIORef botState
            , EchoBot.hModifyState = modifyIORef' botState
            }
      }
