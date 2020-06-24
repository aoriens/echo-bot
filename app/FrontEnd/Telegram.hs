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
    (lastId, inMessages) <- receiveMessages h nextUpdateId
    forM_ inMessages $ sendMessageToBotAndHandleOutput h
    writeIORef nextUpdateIdRef $ succ lastId

receiveMessages :: Handle -> UpdateId -> IO (UpdateId, [Message])
receiveMessages h nextUpdateId = do
  result <-
    getResponseWithMethod
      h
      (ApiMethod "getUpdates")
      (A.object ["offset" .= nextUpdateId, "timeout" .= (25 :: Int)])
      parseUpdatesResponse
  pure $ first (fromMaybe nextUpdateId) result

getResponseWithMethod ::
     (A.ToJSON request, A.FromJSON response)
  => Handle
  -> ApiMethod
  -> request
  -> (A.Value -> A.Parser response)
  -> IO response
getResponseWithMethod h method request parser = do
  Logger.debug h $ "Send " .< method <> ": " .< A.encode request
  response <- getResponse
  Logger.debug h $ "Responded with " .< Client.responseBody response
  result <- getResult response
  logResult result
  pure $ either error id result
  where
    getResponse = do
      httpRequest <- getRequest
      Client.httpLbs httpRequest $ hHttpManager h
    getRequest = do
      httpRequest <- Client.requestFromURI $ endpointURI h method
      pure
        httpRequest
          { Client.checkResponse = Client.throwErrorStatusCodes
          , Client.method = "POST"
          , Client.requestHeaders = [("Content-Type", "application/json")]
          , Client.requestBody = Client.RequestBodyLBS $ A.encode request
          }
    getResult response =
      pure $ do
        value <- A.eitherDecode (Client.responseBody response)
        A.parseEither parser value
    logResult (Left e) = Logger.error h $ "Response error: " <> T.pack e
    logResult _ = pure ()

getResponseWithMethod_ ::
     (A.ToJSON request) => Handle -> ApiMethod -> request -> IO ()
getResponseWithMethod_ h method request =
  getResponseWithMethod h method request (const $ pure ())

sendMessageToBotAndHandleOutput :: Handle -> Message -> IO ()
sendMessageToBotAndHandleOutput h message = do
  response <-
    EchoBot.respond (hBotHandle h) . EchoBot.ReplyRequest . messageText $
    message
  case response of
    EchoBot.RepliesResponse texts -> mapM_ (sendMessage h chatId) texts
    EchoBot.MenuResponse _ _ ->
      sendMessage h chatId "This command has not implemented already"
    EchoBot.EmptyResponse -> pure ()
  where
    chatId = messageChatId message

sendMessage :: Handle -> ChatId -> Text -> IO ()
sendMessage h chatId text = do
  getResponseWithMethod_
    h
    (ApiMethod "sendMessage")
    (A.object ["chat_id" .= chatId, "text" .= text])

newtype UpdateId =
  UpdateId Int
  deriving (A.FromJSON, A.ToJSON, Enum, Show)

parseUpdatesResponse :: A.Value -> A.Parser (Maybe UpdateId, [Message])
parseUpdatesResponse =
  A.withObject "response" $ \r -> do
    updates <- r .: "result"
    (listToMaybe . reverse *** catMaybes) . unzip <$>
      traverse parseUpdate updates
  where
    parseUpdate =
      A.withObject "update" $ \update ->
        liftA2 (,) (update .: "update_id") (optional $ update .: "message")

data Message =
  Message
    { messageText :: Text
    , messageChatId :: ChatId
    }

instance A.FromJSON Message where
  parseJSON =
    A.withObject "Message" $ \m -> do
      text <- m .: "text"
      chat <- m .: "chat"
      chatId <- chat .: "id"
      pure Message {messageText = text, messageChatId = chatId}

newtype ChatId =
  ChatId Int
  deriving (A.FromJSON, A.ToJSON)

newtype ApiMethod =
  ApiMethod String
  deriving (Show)

endpointURI :: Handle -> ApiMethod -> URI.URI
endpointURI h (ApiMethod method) =
  fromMaybe (error $ "Bad URI: " ++ uri) . URI.parseURI $ uri
  where
    uri = "https://api.telegram.org/bot" ++ apiToken ++ "/" ++ method
    apiToken = T.unpack . confApiToken . hConfig $ h

instance Logger.Logger Handle IO where
  lowLevelLog = Logger.lowLevelLog . hLogHandle
