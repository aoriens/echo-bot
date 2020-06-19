{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Telegram
  ( run
  , Handle(..)
  , Config(..)
  ) where

import Control.Monad
import qualified Data.Aeson as A
import Data.Aeson ((.:))
import qualified Data.Aeson.Types as A
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable
import qualified EchoBot
import qualified Logger
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.URI as URI

newtype Config =
  Config
    { confApiToken :: Text
    }

data Handle =
  Handle
    { hBotHandle :: EchoBot.Handle IO
    , hLogHandle :: Logger.Handle IO
    , hConfig :: Config
    }

run :: Handle -> IO ()
run h = do
  httpManager <- Client.newManager TLS.tlsManagerSettings
  forever $ do
    inputs <- receiveMessages h httpManager
    forM_ inputs $ \input ->
      sendRequestToBotAndHandleOutput h . EchoBot.ReplyRequest $ input

receiveMessages :: Handle -> Client.Manager -> IO [Text]
receiveMessages h httpManager = do
  Logger.debug (hLogHandle h) "Pulling new messages..."
  response <- getResponse
  let body = Client.responseBody response
      result = A.parseEither parseMessages =<< A.eitherDecode body
  logResult result
  pure $ either (const []) id result
  where
    getResponse = do
      request <- getRequest
      Client.httpLbs request httpManager
    getRequest = do
      request <- Client.requestFromURI $ endpointURI h "getUpdates"
      pure $ Client.setRequestCheckStatus request
    logResult (Left e) =
      Logger.error (hLogHandle h) $ "Response error: " <> T.pack e
    logResult (Right messages) =
      Logger.info (hLogHandle h) $
      "Received " <> T.pack (show $ length messages) <> " messages"

sendRequestToBotAndHandleOutput :: Handle -> EchoBot.Request -> IO ()
sendRequestToBotAndHandleOutput h request = do
  response <- EchoBot.respond (hBotHandle h) request
  case response of
    EchoBot.RepliesResponse texts -> sendMessages texts
    EchoBot.MenuResponse title opts -> sendMenu h title opts
    EchoBot.EmptyResponse -> pure ()

sendMessages :: [Text] -> IO ()
sendMessages
  -- Temporarily output them to stdout
 = mapM_ (T.putStrLn . (">> " <>))

sendMenu :: Handle -> Text -> [(Int, EchoBot.Request)] -> IO ()
sendMenu _ _ _ = undefined

parseMessages :: A.Value -> A.Parser [Text]
parseMessages =
  A.withObject "response" $ \r -> do
    events <- r .: "result"
    for events $
      A.withObject "event" $ \event -> do
        message <- event .: "message"
        message .: "text"

endpointURI :: Handle -> String -> URI.URI
endpointURI h method =
  fromMaybe (error $ "Bad URI: " ++ uri) . URI.parseURI $ uri
  where
    uri = "https://api.telegram.org/bot" ++ apiToken ++ "/" ++ method
    apiToken = T.unpack . confApiToken . hConfig $ h
