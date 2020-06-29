{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses #-}

-- | The module connects the bot business logic and the Telegram
-- messenger protocol. It is to be isolated from concrete libraries
-- and I/O yet in order to be tested.
module FrontEnd.Telegram.Core
  ( new
  , run
  , Handle
  , Config(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as A
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BS
import Data.IORef
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap.Lazy (IntMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified EchoBot
import qualified Logger
import Logger ((.<))
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.URI as URI

data Config =
  Config
    -- | API token to authorize the Telegram bot. It is to be
    -- registered using Telegram documentation and kept in secret.
    { confApiToken :: Text
    -- | API URL prefix, e.g. https://api.telegram.com/ . A trailing
    -- slash is optional and acceptable.
    , confURLPrefix :: Text
    -- | Poll timeout in seconds. The telegram server will wait that
    -- much before returning an empty list of events. A good value is
    -- large: it reduces server load and clutter in debug logs. A
    -- smaller value (e.g. zero) is convenient for debugging.
    , confPollTimeout :: Int
    -- | Connection timeout in seconds. It is considered an error when
    -- no response headers are accepted in this period. An obvious
    -- exception is the poll request: its connection timeout is
    -- confPollTimeout seconds more.
    , confConnectionTimeout :: Int
    }

data Handle =
  Handle
    { hBotHandle :: EchoBot.Handle IO
    , hLogHandle :: Logger.Handle IO
    , hHttpManager :: HTTP.Manager
    -- | Open menus keyed by ChatId. Each chat can have a dedicated
    -- open menu.
    , hOpenMenus :: IORef (IntMap OpenMenu)
    -- Fields copied from Config and optionally cleaned up or
    -- canonicalized
    , hApiToken :: Text
    , hURLPrefixWithoutTrailingSlash :: String
    , hPollTimeout :: Int
    , hConnectionTimeout :: Int
    }

-- | Data to describe the currently open menu in a specific chat.
data OpenMenu =
  OpenMenu
    { omMessageId :: MessageId
    , omTitle :: Text
    , omChoiceMap :: [(CallbackData, EchoBot.Event)]
    }

-- | HTTP request value, independent of specific libraries.
data HttpRequest =
  HttpRequest
    { hrMethod :: HttpMethod
    , hrURI :: String
    , hrHeaders :: [(String, String)]
    , hrBody :: BS.ByteString
    , hrResponseTimeout :: Int
    }

data HttpMethod =
  POST

new :: EchoBot.Handle IO -> Logger.Handle IO -> Config -> IO Handle
new botHandle logHandle config = do
  httpManager <- HTTP.newManager TLS.tlsManagerSettings
  menus <- newIORef mempty
  pure
    Handle
      { hBotHandle = botHandle
      , hLogHandle = logHandle
      , hHttpManager = httpManager
      , hOpenMenus = menus
      , hApiToken = confApiToken config
      , hURLPrefixWithoutTrailingSlash =
          T.unpack . T.dropWhileEnd (== '/') $ confURLPrefix config
      , hPollTimeout = confPollTimeout config
      , hConnectionTimeout = confConnectionTimeout config
      }

run :: Handle -> IO ()
run h = do
  nextUpdateIdRef <- newIORef $ UpdateId 0
  forever $ do
    nextUpdateId <- readIORef nextUpdateIdRef
    (lastId, events) <- receiveEvents h nextUpdateId
    forM_ events $ handleEvent h
    whenJust lastId $ writeIORef nextUpdateIdRef . succ

receiveEvents :: Handle -> UpdateId -> IO (Maybe UpdateId, [Event])
receiveEvents h nextUpdateId = do
  getResponseWithMethodAndRequestModifier
    h
    (ApiMethod "getUpdates")
    (A.object ["offset" .= nextUpdateId, "timeout" .= pollTimeout])
    (\request ->
       request {hrResponseTimeout = hrResponseTimeout request + pollTimeout})
    parseUpdatesResponse
  where
    pollTimeout = hPollTimeout h

handleEvent :: Handle -> Event -> IO ()
handleEvent h (MessageEvent message) =
  sendRequestToBotAndHandleOutput
    h
    (messageChatId message)
    (EchoBot.MessageEvent $ messageText message)
handleEvent h (MenuChoiceEvent callbackQuery) =
  void . runMaybeT $ do
    confirmToServer
    menu <- findOpenMenuOrExit
    exitIfBadMessageId menu
    botRequest <- findBotRequestMatchingChoiceOrExit menu
    lift $ do
      closeMenuWithReplacementText
        h
        (cqChatId callbackQuery)
        "You have already made your choice"
      sendRequestToBotAndHandleOutput h chatId botRequest
  where
    confirmToServer =
      lift . sendAnswerCallbackQueryRequest h $ cqId callbackQuery
    findOpenMenuOrExit = do
      menus <- lift . readIORef $ hOpenMenus h
      maybe exitWithNoOpenMenu pure $ IntMap.lookup menuKey menus
    exitIfBadMessageId menu =
      when (cqMessageId callbackQuery /= omMessageId menu) $
      exitWithBadMessageId menu
    findBotRequestMatchingChoiceOrExit menu =
      maybe exitWithWrongButton pure . lookup (cqData callbackQuery) $
      omChoiceMap menu
    exitWithNoOpenMenu = do
      lift . Logger.warn h $
        "There is no active menu for " .< chatId <>
        ", but menu choice query is given: " .< callbackQuery
      empty
    exitWithBadMessageId menu = do
      lift . Logger.warn h $
        "MessageId from " .< callbackQuery <>
        " mismatches open menu: " .< omMessageId menu
      empty
    exitWithWrongButton = do
      lift . Logger.warn h $ "Invalid menu choice: " .< callbackQuery
      empty
    menuKey = unChatId chatId
    chatId = cqChatId callbackQuery

sendRequestToBotAndHandleOutput :: Handle -> ChatId -> EchoBot.Event -> IO ()
sendRequestToBotAndHandleOutput h chatId request = do
  response <- EchoBot.respond (hBotHandle h) request
  case response of
    EchoBot.RepliesResponse texts -> mapM_ (sendMessage h chatId) texts
    EchoBot.MenuResponse title opts -> openMenu h chatId title opts
    EchoBot.EmptyResponse -> pure ()

-- | Sends a menu with repetition count options. Currently no other
-- menus are implemented.
openMenu :: Handle -> ChatId -> Text -> [(Int, EchoBot.Event)] -> IO ()
openMenu h chatId title opts = do
  closeMenuWithReplacementText h chatId "The menu is now out-of-date"
  Logger.info h "Sending a message with menu"
  messageId <-
    sendMessageWithInlineKeyboard h chatId title [zip callbackDataList labels]
  modifyIORef' (hOpenMenus h) $
    IntMap.insert (unChatId chatId) (makeMenu messageId)
  where
    callbackDataList = map (CallbackData . T.pack . show) ([0 ..] :: [Int])
    labels = map (T.pack . show . fst) opts
    makeMenu messageId =
      OpenMenu
        { omMessageId = messageId
        , omTitle = title
        , omChoiceMap = zip callbackDataList $ map snd opts
        }

-- | Safely deletes menu both from the chat and from the pending menu
-- table.
closeMenuWithReplacementText :: Handle -> ChatId -> Text -> IO ()
closeMenuWithReplacementText h chatId replacementText = do
  menus <- readIORef $ hOpenMenus h
  let (maybeMenu, menus') =
        IntMap.updateLookupWithKey (\_ _ -> Nothing) menuKey menus
  whenJust maybeMenu $ \menu -> do
    Logger.info h $ "Closing menu with replacement text: " <> replacementText
    writeIORef (hOpenMenus h) menus'
    deleteMenuFromChat menu
  where
    menuKey = unChatId chatId
    deleteMenuFromChat menu =
      sendEditMessageTextRequest
        h
        chatId
        (omMessageId menu)
        (makeEditedTitle menu)
    makeEditedTitle menu = omTitle menu <> "\n(" <> replacementText <> ")"

sendMessage :: Handle -> ChatId -> Text -> IO ()
sendMessage h chatId text = do
  executeMethod
    h
    (ApiMethod "sendMessage")
    (A.object ["chat_id" .= chatId, "text" .= text])

sendMessageWithInlineKeyboard ::
     Handle -> ChatId -> Text -> [[(CallbackData, Text)]] -> IO MessageId
sendMessageWithInlineKeyboard h chatId title opts =
  getResponseWithMethod h (ApiMethod "sendMessage") request parseMessageId
  where
    request =
      A.object
        [ "chat_id" .= chatId
        , "text" .= title
        , "reply_markup" .=
          A.object ["inline_keyboard" .= map (map makeButton) opts]
        ]
    makeButton (callbackData, text) =
      A.object ["callback_data" .= callbackData, "text" .= text]
    parseMessageId =
      A.withObject "response" $ \r -> do
        message <- r .: "result"
        message .: "message_id"

sendAnswerCallbackQueryRequest :: Handle -> CallbackQueryId -> IO ()
sendAnswerCallbackQueryRequest h queryId =
  executeMethod
    h
    (ApiMethod "answerCallbackQuery")
    (A.object ["callback_query_id" .= queryId])

sendEditMessageTextRequest :: Handle -> ChatId -> MessageId -> Text -> IO ()
sendEditMessageTextRequest h chatId messageId text =
  executeMethod
    h
    (ApiMethod "editMessageText")
    (A.object ["chat_id" .= chatId, "message_id" .= messageId, "text" .= text])

getResponseWithMethod ::
     (A.ToJSON request)
  => Handle
  -> ApiMethod
  -> request
  -> (A.Value -> A.Parser response)
  -> IO response
getResponseWithMethod h method request =
  getResponseWithMethodAndRequestModifier h method request id

getResponseWithMethodAndRequestModifier ::
     (A.ToJSON request)
  => Handle
  -> ApiMethod
  -> request
  -> (HttpRequest -> HttpRequest)
  -> (A.Value -> A.Parser response)
  -> IO response
getResponseWithMethodAndRequestModifier h method request httpRequestModifier parser = do
  Logger.debug h $ "Send " .< method <> ": " .< A.encode request
  response <- getHttpResponse h httpRequest
  Logger.debug h $ "Responded with " .< response
  result <- getResult response
  logResult result
  pure $ either error id result
  where
    httpRequest =
      httpRequestModifier
        HttpRequest
          { hrMethod = POST
          , hrURI = endpointURI h method
          , hrHeaders = [("Content-Type", "application/json")]
          , hrBody = A.encode request
          , hrResponseTimeout = hConnectionTimeout h
          }
    getResult response =
      pure $ do
        value <- A.eitherDecode response
        A.parseEither parser value
    logResult (Left e) = Logger.error h $ "Response error: " <> T.pack e
    logResult _ = pure ()

getHttpResponse :: Handle -> HttpRequest -> IO BS.ByteString
getHttpResponse h request = do
  httpRequest <- configureRequest <$> HTTP.requestFromURI uri
  HTTP.responseBody <$> HTTP.httpLbs httpRequest (hHttpManager h)
  where
    configureRequest httpRequest =
      httpRequest
        { HTTP.checkResponse = HTTP.throwErrorStatusCodes
        , HTTP.method = methodString $ hrMethod request
        , HTTP.requestHeaders =
            map (fromString *** fromString) $ hrHeaders request
        , HTTP.requestBody = HTTP.RequestBodyLBS $ hrBody request
        , HTTP.responseTimeout =
            HTTP.responseTimeoutMicro . (1000000 *) $ hrResponseTimeout request
        }
    uri =
      fromMaybe (error $ "Bad URI: " ++ uriString) . URI.parseURI $ uriString
    uriString = hrURI request
    methodString POST = "POST"

executeMethod :: (A.ToJSON request) => Handle -> ApiMethod -> request -> IO ()
executeMethod h method request =
  getResponseWithMethod h method request (const $ pure ())

endpointURI :: Handle -> ApiMethod -> String
endpointURI h (ApiMethod method) =
  hURLPrefixWithoutTrailingSlash h ++ "/bot" ++ apiToken ++ "/" ++ method
  where
    apiToken = T.unpack $ hApiToken h

instance Logger.Logger Handle IO where
  lowLevelLog = Logger.lowLevelLog . hLogHandle

parseUpdatesResponse :: A.Value -> A.Parser (Maybe UpdateId, [Event])
parseUpdatesResponse =
  A.withObject "response" $ \r -> do
    updates <- r .: "result"
    (listToMaybe . reverse *** catMaybes) . unzip <$>
      traverse parseUpdate updates
  where
    parseUpdate =
      A.withObject "update" $ \update ->
        liftA2 (,) (update .: "update_id") (optional $ parseEvent update)
    parseEvent update =
      (MessageEvent <$> update .: "message") <|>
      (MenuChoiceEvent <$> update .: "callback_query") <|>
      fail "unsupported kind of 'Update' object"

-- | An event that can occur in the chat, caused by the user.
data Event
  = MessageEvent Message
  | MenuChoiceEvent CallbackQuery

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

data CallbackQuery =
  CallbackQuery
    { cqMessageId :: MessageId
    , cqData :: CallbackData
    , cqId :: CallbackQueryId
    , cqChatId :: ChatId
    }
  deriving (Show)

instance A.FromJSON CallbackQuery where
  parseJSON =
    A.withObject "CallbackQuery" $ \o -> do
      message <- o .: "message"
      data' <- o .: "data"
      queryId <- o .: "id"
      messageId <- message .: "message_id"
      chat <- message .: "chat"
      chatId <- chat .: "id"
      pure
        CallbackQuery
          { cqMessageId = messageId
          , cqData = data'
          , cqId = queryId
          , cqChatId = chatId
          }

newtype ChatId =
  ChatId
    { unChatId :: Int
    }
  deriving (Show, A.FromJSON, A.ToJSON)

newtype UpdateId =
  UpdateId Int
  deriving (A.FromJSON, A.ToJSON, Enum, Show)

newtype MessageId =
  MessageId Int
  deriving (Eq, Show, A.FromJSON, A.ToJSON)

newtype CallbackQueryId =
  CallbackQueryId Text
  deriving (Show, A.FromJSON, A.ToJSON)

newtype CallbackData =
  CallbackData Text
  deriving (Eq, Show, A.FromJSON, A.ToJSON)

newtype ApiMethod =
  ApiMethod String
  deriving (Show)

whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust x f = maybe (pure ()) f x