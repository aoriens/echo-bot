{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import App
import Data.IORef
import qualified EchoBot
import qualified FrontEnd.Console

main :: IO ()
main = do
  botState <- newIORef EchoBot.makeState
  runApp FrontEnd.Console.run Env {envBotState = botState}
