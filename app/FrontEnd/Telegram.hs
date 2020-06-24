{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses #-}

module FrontEnd.Telegram
  ( new
  , run
  , Handle
  , Config(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Data.Aeson as A
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Types as A
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified EchoBot
import qualified Logger
import Logger ((.<))
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
    , hHttpManager :: Client.Manager
    }

new :: EchoBot.Handle IO -> Logger.Handle IO -> Config -> IO Handle
new botHandle logHandle config = do
  httpManager <- Client.newManager TLS.tlsManagerSettings
  pure
    Handle
      { hBotHandle = botHandle
      , hLogHandle = logHandle
      , hConfig = config
      , hHttpManager = httpManager
      }

run :: Handle -> IO ()
run h = do
  nextUpdateIdRef <- newIORef $ UpdateId 0
  forever $ do
    nextUpdateId <- readIORef nextUpdateIdRef
    (lastId, inputs) <- receiveMessages h nextUpdateId
    forM_ inputs $ sendRequestToBotAndHandleOutput h . EchoBot.ReplyRequest
    writeIORef nextUpdateIdRef $ succ lastId

receiveMessages :: Handle -> UpdateId -> IO (UpdateId, [Text])
receiveMessages h nextUpdateId = do
  Logger.debug h $ "getUpdates: " .< requestBody
  response <- getResponse
  Logger.debug h $ "Responded with " .< Client.responseBody response
  let result = decodeResponse response
  logResult result
  pure $
    either (const (nextUpdateId, [])) (first $ fromMaybe nextUpdateId) result
  where
    getResponse = do
      request <- getRequest
      Client.httpLbs request $ hHttpManager h
    getRequest = do
      request <- Client.requestFromURI $ endpointURI h "getUpdates"
      pure
        request
          { Client.checkResponse = Client.throwErrorStatusCodes
          , Client.method = "POST"
          , Client.requestHeaders = [("Content-Type", "application/json")]
          , Client.requestBody = Client.RequestBodyLBS requestBody
          }
    requestBody =
      A.encode $ A.object ["offset" .= nextUpdateId, "timeout" .= (25 :: Int)]
    decodeResponse response = do
      value <- A.eitherDecode $ Client.responseBody response
      A.parseEither parseUpdatesResponse value
    logResult (Left e) = Logger.error h $ "Response error: " <> T.pack e
    logResult (Right (maybeId, messages)) = do
      Logger.info h $ "Received " .< length messages <> " messages"
      Logger.debug h $ "Received last update_id=" .< maybeId

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

newtype UpdateId =
  UpdateId Int
  deriving (A.FromJSON, A.ToJSON, Enum, Show)

parseUpdatesResponse :: A.Value -> A.Parser (Maybe UpdateId, [Text])
parseUpdatesResponse =
  A.withObject "response" $ \r -> do
    updates <- r .: "result"
    retainLastIdOnly . catMaybes <$> traverse parseMessage updates
  where
    parseMessage :: A.Value -> A.Parser (Maybe (UpdateId, Text))
    parseMessage =
      A.withObject "update" $ \update -> do
        updateId <- update .: "update_id"
        optional $ do
          message <- update .: "message"
          text <- message .: "text"
          pure (updateId, text)
    retainLastIdOnly :: [(UpdateId, Text)] -> (Maybe UpdateId, [Text])
    retainLastIdOnly = first (listToMaybe . reverse) . unzip

endpointURI :: Handle -> String -> URI.URI
endpointURI h method =
  fromMaybe (error $ "Bad URI: " ++ uri) . URI.parseURI $ uri
  where
    uri = "https://api.telegram.org/bot" ++ apiToken ++ "/" ++ method
    apiToken = T.unpack . confApiToken . hConfig $ h

instance Logger.Logger Handle IO where
  lowLevelLog = Logger.lowLevelLog . hLogHandle
