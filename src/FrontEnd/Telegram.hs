{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleInstances #-}

-- | The module connects the bot business logic and the Telegram
-- messenger protocol. It is to be isolated from concrete libraries
-- and I/O yet in order to be tested.
module FrontEnd.Telegram
  ( makeState
  , run
  , Handle(..)
  , State
  , Config(..)
  , HttpRequest(..)
  , HttpMethod(..)
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
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap.Lazy (IntMap)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified EchoBot
import qualified Logger
import Logger ((.<))

data Config =
  Config
    -- | API token to authorize the Telegram bot. It is to be
    -- registered using Telegram documentation and kept in secret.
    { confApiToken :: Text
    -- | API URL prefix, e.g. https://api.telegram.com . It is not
    -- assumed to have a trailing slash.
    , confURLPrefixWithoutTrailingSlash :: String
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

data Handle m =
  Handle
    { hBotHandle :: EchoBot.Handle m
    , hLogHandle :: Logger.Handle m
    , hGetHttpResponse :: HttpRequest -> m BS.ByteString
    , hGetState :: m State
    , hModifyState' :: (State -> State) -> m ()
    , hConfig :: Config
    }

newtype State =
  State
      -- | Open menus keyed by ChatId. Each chat can have a dedicated
      -- open menu.
    { stOpenMenus :: IntMap OpenMenu
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

makeState :: State
makeState = State mempty

run :: Monad m => Handle m -> m ()
run h = go $ UpdateId 0
  where
    go nextUpdateId = do
      (lastId, events) <- receiveEvents h nextUpdateId
      forM_ events $ handleEvent h
      go $ maybe nextUpdateId succ lastId

receiveEvents :: Monad m => Handle m -> UpdateId -> m (Maybe UpdateId, [Event])
receiveEvents h nextUpdateId = do
  getResponseWithMethodAndRequestModifier
    h
    (ApiMethod "getUpdates")
    (A.object ["offset" .= nextUpdateId, "timeout" .= pollTimeout])
    (\request ->
       request {hrResponseTimeout = hrResponseTimeout request + pollTimeout})
    parseUpdatesResponse
  where
    pollTimeout = confPollTimeout $ hConfig h

handleEvent :: Monad m => Handle m -> Event -> m ()
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
      menus <- stOpenMenus <$> lift (hGetState h)
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

sendRequestToBotAndHandleOutput ::
     Monad m => Handle m -> ChatId -> EchoBot.Event -> m ()
sendRequestToBotAndHandleOutput h chatId request = do
  response <- EchoBot.respond (hBotHandle h) request
  case response of
    EchoBot.RepliesResponse texts -> mapM_ (sendMessage h chatId) texts
    EchoBot.MenuResponse title opts -> openMenu h chatId title opts
    EchoBot.EmptyResponse -> pure ()

-- | Sends a menu with repetition count options. Currently no other
-- menus are implemented.
openMenu ::
     Monad m => Handle m -> ChatId -> Text -> [(Int, EchoBot.Event)] -> m ()
openMenu h chatId title opts = do
  closeMenuWithReplacementText h chatId "The menu is now out-of-date"
  Logger.info h "Sending a message with menu"
  messageId <-
    sendMessageWithInlineKeyboard h chatId title [zip callbackDataList labels]
  hModifyState' h $
    State . IntMap.insert (unChatId chatId) (makeMenu messageId) . stOpenMenus
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
closeMenuWithReplacementText :: Monad m => Handle m -> ChatId -> Text -> m ()
closeMenuWithReplacementText h chatId replacementText = do
  menus <- stOpenMenus <$> hGetState h
  let (maybeMenu, menus') =
        IntMap.updateLookupWithKey (\_ _ -> Nothing) menuKey menus
  whenJust maybeMenu $ \menu -> do
    Logger.info h $ "Closing menu with replacement text: " <> replacementText
    hModifyState' h $ const (State menus')
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

sendMessage :: Monad m => Handle m -> ChatId -> Text -> m ()
sendMessage h chatId text = do
  executeMethod
    h
    (ApiMethod "sendMessage")
    (A.object ["chat_id" .= chatId, "text" .= text])

sendMessageWithInlineKeyboard ::
     Monad m
  => Handle m
  -> ChatId
  -> Text
  -> [[(CallbackData, Text)]]
  -> m MessageId
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

sendAnswerCallbackQueryRequest :: Monad m => Handle m -> CallbackQueryId -> m ()
sendAnswerCallbackQueryRequest h queryId =
  executeMethod
    h
    (ApiMethod "answerCallbackQuery")
    (A.object ["callback_query_id" .= queryId])

sendEditMessageTextRequest ::
     Monad m => Handle m -> ChatId -> MessageId -> Text -> m ()
sendEditMessageTextRequest h chatId messageId text =
  executeMethod
    h
    (ApiMethod "editMessageText")
    (A.object ["chat_id" .= chatId, "message_id" .= messageId, "text" .= text])

getResponseWithMethod ::
     (A.ToJSON request, Monad m)
  => Handle m
  -> ApiMethod
  -> request
  -> (A.Value -> A.Parser response)
  -> m response
getResponseWithMethod h method request =
  getResponseWithMethodAndRequestModifier h method request id

getResponseWithMethodAndRequestModifier ::
     (A.ToJSON request, Monad m)
  => Handle m
  -> ApiMethod
  -> request
  -> (HttpRequest -> HttpRequest)
  -> (A.Value -> A.Parser response)
  -> m response
getResponseWithMethodAndRequestModifier h method request httpRequestModifier parser = do
  Logger.debug h $ "Send " .< method <> ": " .< A.encode request
  response <- hGetHttpResponse h httpRequest
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
          , hrResponseTimeout = confConnectionTimeout $ hConfig h
          }
    getResult response =
      pure $ do
        value <- A.eitherDecode response
        A.parseEither parser value
    logResult (Left e) = Logger.error h $ "Response error: " <> T.pack e
    logResult _ = pure ()

executeMethod ::
     (A.ToJSON request, Monad m) => Handle m -> ApiMethod -> request -> m ()
executeMethod h method request =
  getResponseWithMethod h method request (const $ pure ())

endpointURI :: Handle m -> ApiMethod -> String
endpointURI h (ApiMethod method) =
  urlPrefix ++ "/bot" ++ apiToken ++ "/" ++ method
  where
    urlPrefix = confURLPrefixWithoutTrailingSlash (hConfig h)
    apiToken = T.unpack . confApiToken $ hConfig h

instance Logger.Logger (Handle m) m where
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