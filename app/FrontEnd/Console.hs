{-# LANGUAGE OverloadedStrings #-}

-- | The console front-end is responsible for console I/O and
-- appropriate handling of other high-level bot interactions (menu
-- output etc).
module FrontEnd.Console
  ( run
  ) where

import App
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO
import qualified EchoBot
import System.IO

run :: App ()
run = do
  liftIO $ TIO.putStrLn "Welcome to the echo-bot"
  forever $ do
    input <- liftIO $ getLineWithPrompt "> "
    sendRequestToBotAndHandleOutput . EchoBot.ReplyRequest $ input

sendRequestToBotAndHandleOutput :: EchoBot.Request -> App ()
sendRequestToBotAndHandleOutput request = do
  response <- EchoBot.respond request
  case response of
    EchoBot.RepliesResponse texts -> liftIO $ mapM_ TIO.putStrLn texts
    EchoBot.MenuResponse title opts -> handleMenuResponse title opts
    EchoBot.EmptyResponse -> pure ()

getLineWithPrompt :: Text -> IO Text
getLineWithPrompt prompt = do
  TIO.putStr prompt
  hFlush stdout
  TIO.getLine

handleMenuResponse :: Text -> [(Text, EchoBot.ChoiceId)] -> App ()
handleMenuResponse title opts = do
  liftIO . TIO.putStrLn . renderMenu title . map fst $ opts
  choiceId <- liftIO $ readUserChoice "Choice> " opts
  sendRequestToBotAndHandleOutput . EchoBot.MenuChoiceRequest $ choiceId

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
