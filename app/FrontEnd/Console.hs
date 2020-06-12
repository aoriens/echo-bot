{-# LANGUAGE OverloadedStrings #-}

-- | The console front-end is responsible for console I/O and
-- appropriate handling of other high-level bot interactions (menu
-- output etc).
module FrontEnd.Console
  ( run
  , Handle(..)
  ) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO
import qualified EchoBot
import System.IO hiding (Handle)

newtype Handle =
  Handle
    { hBotHandle :: EchoBot.Handle IO
    }

run :: Handle -> IO ()
run h = do
  TIO.putStrLn "Welcome to the echo-bot"
  forever $ do
    input <- getLineWithPrompt "> "
    sendRequestToBotAndHandleOutput h . EchoBot.ReplyRequest $ input

sendRequestToBotAndHandleOutput :: Handle -> EchoBot.Request -> IO ()
sendRequestToBotAndHandleOutput h request = do
  response <- EchoBot.respond (hBotHandle h) request
  case response of
    EchoBot.RepliesResponse texts -> mapM_ TIO.putStrLn texts
    EchoBot.MenuResponse title opts -> handleMenuResponse h title opts
    EchoBot.EmptyResponse -> pure ()

getLineWithPrompt :: Text -> IO Text
getLineWithPrompt prompt = do
  TIO.putStr prompt
  hFlush stdout
  TIO.getLine

handleMenuResponse :: Handle -> Text -> [(Text, EchoBot.Request)] -> IO ()
handleMenuResponse h title opts = do
  TIO.putStrLn . renderMenu title . map fst $ opts
  request <- readUserChoice "Choice> " opts
  sendRequestToBotAndHandleOutput h request

readUserChoice :: Text -> [(Text, a)] -> IO a
readUserChoice prompt opts = go
  where
    go = do
      input <- getLineWithPrompt prompt
      maybe go pure $ lookup (T.strip input) opts

renderMenu :: Text -> [Text] -> Text
renderMenu title options = title <> "\n" <> formattedOptions
  where
    formattedOptions = T.intercalate "\n" . map formatOption $ options
    formatOption text = "  - " <> text
