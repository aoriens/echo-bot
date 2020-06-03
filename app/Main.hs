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
    input <- getLineWithPrompt "> "
    handleBotOutput . EchoBot.respond . EchoBot.InMessage $ input

handleBotOutput :: EchoBot.Response -> IO ()
handleBotOutput response = do
  case response of
    EchoBot.OutText outText -> TIO.putStrLn outText
    EchoBot.OutMenu title opts -> handleMenuResponse title opts
    EchoBot.OutNothing -> pure ()

getLineWithPrompt :: Text -> IO Text
getLineWithPrompt prompt = do
  TIO.putStr prompt
  hFlush stdout
  TIO.getLine

handleMenuResponse :: Text -> [(Text, EchoBot.ChoiceId)] -> IO ()
handleMenuResponse title opts = do
  TIO.putStrLn . renderMenu title . map fst $ opts
  choiceId <- readUserChoice "Choice> " opts
  handleBotOutput . EchoBot.respond . EchoBot.InMenuChoice $ choiceId

readUserChoice :: Text -> [(Text, a)] -> IO a
readUserChoice prompt opts = go
  where
    go = do
      input <- getLineWithPrompt prompt
      maybe go pure $ lookup (T.strip input) opts

renderMenu :: Text -> [Text] -> Text
renderMenu title options = title `T.snoc` '\n' `T.append` formattedOptions
  where
    formattedOptions = T.intercalate "\n" . map formatOption $ options
    formatOption text = "  - " `T.append` text
