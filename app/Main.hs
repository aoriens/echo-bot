{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO
import qualified EchoBot
import System.IO

main :: IO ()
main = do
  TIO.putStrLn "Welcome to the echo-bot"
  forever $ do
    TIO.putStr "> "
    hFlush stdout
    TIO.putStrLn . handleInput =<< TIO.getLine
  where
    handleInput = showResponse . EchoBot.respond . EchoBot.InMessage

showResponse :: EchoBot.Response -> Text
showResponse (EchoBot.OutText text) = text
showResponse (EchoBot.OutMenu text options) =
  text `T.snoc` '\n' `T.append` formattedOptions
  where
    formattedOptions = T.intercalate "\n" $ map formatOption options
    formatOption opt = "  - " `T.append` opt
