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
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as A
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Types as A
import Data.IORef
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap.Lazy (IntMap)
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
    -- | Open menus keyed by ChatId. Each chat can have a dedicated
    -- open menu.
    , hOpenMenus :: IORef (IntMap OpenMenu)
    }

-- | Data to describe the currently open menu in a specific chat.
data OpenMenu =
  OpenMenu
    { omMessageId :: MessageId
    , omTitle :: Text
    , omChoiceMap :: [(CallbackData, EchoBot.Request)]
    }

new :: EchoBot.Handle IO -> Logger.Handle IO -> Config -> IO Handle
new botHandle logHandle config = do
  httpManager <- Client.newManager TLS.tlsManagerSettings
  menus <- newIORef mempty
  pure
    Handle
      { hBotHandle = botHandle
      , hLogHandle = logHandle
      , hConfig = config
      , hHttpManager = httpManager
      , hOpenMenus = menus
      }

run :: Handle -> IO ()
run h = do
  nextUpdateIdRef <- newIORef $ UpdateId 0
  forever $ do
    nextUpdateId <- readIORef nextUpdateIdRef
    (lastId, events) <- receiveEvents h nextUpdateId
    forM_ events $ handleEvent h
    case lastId of
      Nothing -> pure ()
      Just ident -> writeIORef nextUpdateIdRef $ succ ident

receiveEvents :: Handle -> UpdateId -> IO (Maybe UpdateId, [Event])
receiveEvents h nextUpdateId = do
  getResponseWithMethod
    h
    (ApiMethod "getUpdates")
    (A.object ["offset" .= nextUpdateId, "timeout" .= (25 :: Int)])
    parseEvents

handleEvent :: Handle -> Event -> IO ()
handleEvent h (MessageEvent message) =
  sendRequestToBotAndHandleOutput
    h
    (messageChatId message)
    (EchoBot.ReplyRequest $ messageText message)
handleEvent h (MenuChoiceEvent callbackQuery) =
  void . runMaybeT $ do
    menu <- findOpenMenuOrExit
    when (cqMessageId callbackQuery /= omMessageId menu) $
      exitWithBadMessageId menu
    botRequest <- findBotRequestMatchingChoiceOrExit menu
    lift $ do
      sendAnswerCallbackQueryRequest h $ cqId callbackQuery
      closeMenu h $ cqChatId callbackQuery
      sendRequestToBotAndHandleOutput h chatId botRequest
  where
    findOpenMenuOrExit = do
      menus <- lift . readIORef $ hOpenMenus h
      maybe exitWithNoOpenMenu pure $ IntMap.lookup menuKey menus
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

sendRequestToBotAndHandleOutput :: Handle -> ChatId -> EchoBot.Request -> IO ()
sendRequestToBotAndHandleOutput h chatId request = do
  response <- EchoBot.respond (hBotHandle h) request
  case response of
    EchoBot.RepliesResponse texts -> mapM_ (sendMessage h chatId) texts
    EchoBot.MenuResponse title opts -> openMenu h chatId title opts
    EchoBot.EmptyResponse -> pure ()

-- | Sends a menu with repetition count options. Currently no other
-- menus are implemented.
openMenu :: Handle -> ChatId -> Text -> [(Int, EchoBot.Request)] -> IO ()
openMenu h chatId title opts = do
  closeMenu h chatId
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
closeMenu :: Handle -> ChatId -> IO ()
closeMenu h chatId = do
  menus <- readIORef $ hOpenMenus h
  let (maybeMenu, menus') =
        IntMap.updateLookupWithKey (\_ _ -> Nothing) menuKey menus
  maybe
    (pure ())
    (\menu -> do
       writeIORef (hOpenMenus h) menus'
       deleteMenuFromChat menu)
    maybeMenu
  where
    menuKey = unChatId chatId
    deleteMenuFromChat menu =
      sendEditMessageTextRequest h chatId (omMessageId menu) (omTitle menu)

sendMessage :: Handle -> ChatId -> Text -> IO ()
sendMessage h chatId text = do
  getResponseWithMethod_
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
  getResponseWithMethod_
    h
    (ApiMethod "answerCallbackQuery")
    (A.object ["callback_query_id" .= queryId])

sendEditMessageTextRequest :: Handle -> ChatId -> MessageId -> Text -> IO ()
sendEditMessageTextRequest h chatId messageId text =
  getResponseWithMethod_
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

endpointURI :: Handle -> ApiMethod -> URI.URI
endpointURI h (ApiMethod method) =
  fromMaybe (error $ "Bad URI: " ++ uri) . URI.parseURI $ uri
  where
    uri = "https://api.telegram.org/bot" ++ apiToken ++ "/" ++ method
    apiToken = T.unpack . confApiToken . hConfig $ h

instance Logger.Logger Handle IO where
  lowLevelLog = Logger.lowLevelLog . hLogHandle

parseEvents :: A.Value -> A.Parser (Maybe UpdateId, [Event])
parseEvents =
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
