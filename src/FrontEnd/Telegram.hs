{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleInstances, LambdaCase #-}

-- | The module connects the bot business logic and the Telegram
-- messenger protocol. It is to be isolated from concrete libraries
-- and I/O yet in order to be tested.
--
-- The module does not use @EchoBot.respond@. You should use
-- @receiveEvents@, pass the events to @EchoBot.respond@, and pass bot
-- responses to @handleBotResponses@.
module FrontEnd.Telegram
  ( makeState
  , receiveEvents
  , handleBotResponse
  , Handle(..)
  , State
  , Config(..)
  , ChatId(..)
  , HttpRequest(..)
  , HttpMethod(..)
  , HttpResult
  , HttpResponse(..)
  , HttpError(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Data.Aeson as A
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BS
import Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Ix
import Data.Maybe
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified EchoBot
import qualified Logger
import Logger ((.<))
import qualified Util.FlexibleState as FlexibleState
import Util.FlexibleState (gets, modify')

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
    -- | A suffix to be added to the title of an outdated menu. A menu
    -- is considered outdated if a different menu of the same type is
    -- sent to the user.
    , confOutdatedMenuTitleSuffix :: Text
    }

data Handle m =
  Handle
    { hLogHandle :: Logger.Handle m
    , hGetHttpResponse :: HttpRequest -> m HttpResult
    -- | An action to introduce a delay when event reading fails. It
    -- prevents emitting large amount of failing requests in short
    -- time.
    , hPauseAfterFailedGettingUpdates :: m ()
    , hStateHandle :: FlexibleState.Handle State m
    , hConfig :: Config
    }

data State =
  State
      -- | Open menus keyed by ChatId. Each chat can have a dedicated
      -- open menu.
    { stOpenMenus :: IntMap OpenMenu
    , stNextUpdateId :: UpdateId
    }

-- | Data to describe the currently open menu in a specific chat.
data OpenMenu =
  OpenMenu
    { omMessageId :: MessageId
    , omTitle :: Text
    , omChoiceMap :: [(CallbackData, EchoBot.Event)]
    }

type HttpResult = Either HttpError HttpResponse

-- | HTTP request value, independent of specific libraries.
data HttpRequest =
  HttpRequest
    { hrMethod :: HttpMethod
    , hrURI :: String
    , hrHeaders :: [(String, String)]
    , hrBody :: BS.ByteString
    -- | A number of seconds to add to the default response timeout.
    -- This is important if the request is expected to wait much time.
    , hrAdditionalResponseTimeout :: Int
    }
  deriving (Show)

data HttpMethod =
  POST
  deriving (Show)

data HttpResponse =
  HttpResponse
    { hrsStatusCode :: Int
    , hrsStatusText :: BS.ByteString
    , hrsBody :: BS.ByteString
    }
  deriving (Show)

newtype HttpError =
  HttpError Text
  deriving (Show)

data RequestError
  = JSONError String
  | HttpStatusError Int BS.ByteString
  | ApiError Text
  | IOError Text
  deriving (Show)

type Failable = Either RequestError

-- | Creates an initial state to be returned from @get $ hStateHandle
-- handle@ until changed.
makeState :: State
makeState = State {stOpenMenus = mempty, stNextUpdateId = UpdateId 0}

-- | Receives new bot events from Telegram and updates the state.
receiveEvents :: Monad m => Handle m -> m [(ChatId, EchoBot.Event)]
receiveEvents h = do
  nextUpdateId <- gets h stNextUpdateId
  r <- receiveLowLevelEvents h nextUpdateId
  case r of
    Right (lastId, events) -> do
      modify' h $ \s -> s {stNextUpdateId = maybe nextUpdateId succ lastId}
      concat <$> mapM (getBotEventFromEvent h) events
    Left _ -> hPauseAfterFailedGettingUpdates h >> pure []

-- | Handles a bot response which is returned by EchoBot and updates
-- the state.
handleBotResponse :: Monad m => Handle m -> ChatId -> EchoBot.Response -> m ()
handleBotResponse h chatId response = do
  case response of
    EchoBot.MessageResponse text -> void $ sendMessage h chatId text
    EchoBot.MenuResponse title opts -> openMenu h chatId title opts

receiveLowLevelEvents ::
     Monad m => Handle m -> UpdateId -> m (Failable (Maybe UpdateId, [Event]))
receiveLowLevelEvents h nextUpdateId = do
  getResponseWithMethodAndRequestModifier
    h
    (ApiMethod "getUpdates")
    (A.object ["offset" .= nextUpdateId, "timeout" .= pollTimeout])
    (\request ->
       request
         { hrAdditionalResponseTimeout =
             hrAdditionalResponseTimeout request + pollTimeout
         })
    parseUpdatesResponse
  where
    pollTimeout = confPollTimeout $ hConfig h

getBotEventFromEvent ::
     Monad m => Handle m -> Event -> m [(ChatId, EchoBot.Event)]
getBotEventFromEvent _ (MessageEvent message) =
  pure [(messageChatId message, EchoBot.MessageEvent $ messageText message)]
getBotEventFromEvent h (MenuChoiceEvent callbackQuery) = do
  confirmToServer
  findOpenMenuForChatId h chatId >>= \case
    Nothing -> endWithNoOpenMenu
    Just menu
      | menuMessageIdIsWrong menu -> endWithBadMessageId menu
      | otherwise ->
        case findBotEventMatchingChoice menu of
          Nothing -> endWithWrongButton
          Just botEvent -> do
            closeMenuAddingTitleSuffix
              h
              (cqChatId callbackQuery)
              "\n(You have already made your choice)"
            pure [(chatId, botEvent)]
  where
    confirmToServer =
      void . sendAnswerCallbackQueryRequest h $ cqId callbackQuery
    menuMessageIdIsWrong menu = cqMessageId callbackQuery /= omMessageId menu
    findBotEventMatchingChoice menu =
      lookup (cqData callbackQuery) $ omChoiceMap menu
    endWithNoOpenMenu = do
      Logger.warn h $
        "There is no active menu for " .< chatId <>
        ", but menu choice query is given: " .< callbackQuery
      pure []
    endWithBadMessageId menu = do
      Logger.warn h $
        "MessageId from " .< callbackQuery <>
        " mismatches open menu: " .< omMessageId menu
      pure []
    endWithWrongButton = do
      Logger.warn h $ "Invalid menu choice: " .< callbackQuery
      pure []
    chatId = cqChatId callbackQuery

findOpenMenuForChatId :: Monad m => Handle m -> ChatId -> m (Maybe OpenMenu)
findOpenMenuForChatId h (ChatId chatId) =
  gets h $ IntMap.lookup chatId . stOpenMenus

-- | Sends a menu with repetition count options. Currently no other
-- menus are implemented.
openMenu ::
     Monad m => Handle m -> ChatId -> Text -> [(Int, EchoBot.Event)] -> m ()
openMenu h chatId title opts = do
  closeMenuAddingTitleSuffix h chatId . confOutdatedMenuTitleSuffix $ hConfig h
  Logger.info h "Sending a message with menu"
  r <-
    sendMessageWithInlineKeyboard h chatId title [zip callbackDataList labels]
  case r of
    Right messageId -> rememberMenuWithMessageId messageId
    Left _ -> pure ()
  where
    callbackDataList = map (CallbackData . T.pack . show) ([0 ..] :: [Int])
    labels = map (T.pack . show . fst) opts
    rememberMenuWithMessageId messageId =
      modify' h $ \s ->
        s
          { stOpenMenus =
              IntMap.insert
                (unChatId chatId)
                (makeMenu messageId)
                (stOpenMenus s)
          }
    makeMenu messageId =
      OpenMenu
        { omMessageId = messageId
        , omTitle = title
        , omChoiceMap = zip callbackDataList $ map snd opts
        }

-- | Safely deletes menu both from the chat and from the pending menu
-- table.
closeMenuAddingTitleSuffix :: Monad m => Handle m -> ChatId -> Text -> m ()
closeMenuAddingTitleSuffix h chatId titleSuffix = do
  menus <- gets h stOpenMenus
  let (maybeMenu, menus') =
        IntMap.updateLookupWithKey (\_ _ -> Nothing) menuKey menus
  whenJust maybeMenu $ \menu -> do
    Logger.info h $ "Closing menu with title suffix: " <> titleSuffix
    modify' h $ \s -> s {stOpenMenus = menus'}
    void $ deleteMenuFromChat menu
  where
    menuKey = unChatId chatId
    deleteMenuFromChat menu =
      sendEditMessageTextRequest
        h
        chatId
        (omMessageId menu)
        (makeEditedTitle menu)
    makeEditedTitle menu = omTitle menu <> titleSuffix

sendMessage :: Monad m => Handle m -> ChatId -> Text -> m (Failable ())
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
  -> m (Failable MessageId)
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
    parseMessageId = A.withObject "result" (.: "message_id")

sendAnswerCallbackQueryRequest ::
     Monad m => Handle m -> CallbackQueryId -> m (Failable ())
sendAnswerCallbackQueryRequest h queryId =
  executeMethod
    h
    (ApiMethod "answerCallbackQuery")
    (A.object ["callback_query_id" .= queryId])

sendEditMessageTextRequest ::
     Monad m => Handle m -> ChatId -> MessageId -> Text -> m (Failable ())
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
  -> m (Failable response)
getResponseWithMethod h method request =
  getResponseWithMethodAndRequestModifier h method request id

executeMethod ::
     (A.ToJSON request, Monad m)
  => Handle m
  -> ApiMethod
  -> request
  -> m (Failable ())
executeMethod h method request =
  getResponseWithMethod h method request (const $ pure ())

getResponseWithMethodAndRequestModifier ::
     (A.ToJSON request, Monad m)
  => Handle m
  -> ApiMethod
  -> request
  -> (HttpRequest -> HttpRequest)
  -> (A.Value -> A.Parser response)
  -> m (Failable response)
getResponseWithMethodAndRequestModifier h method request httpRequestModifier parser = do
  Logger.debug h $
    "Send " <> T.pack (apiMethodName method) <> ": " .< A.encode request
  httpResult <- hGetHttpResponse h httpRequest
  Logger.debug h $ "Responded with " .< httpResult
  let result = decodeHttpResult method parser httpResult
  logResult result
  pure result
  where
    httpRequest =
      httpRequestModifier
        HttpRequest
          { hrMethod = POST
          , hrURI = endpointURI h method
          , hrHeaders = [("Content-Type", "application/json")]
          , hrBody = A.encode request
          , hrAdditionalResponseTimeout = 0
          }
    logResult (Left e) =
      Logger.error h $
      "'" <> T.pack (apiMethodName method) <> "' returned error: " .< e
    logResult _ = pure ()

endpointURI :: Handle m -> ApiMethod -> String
endpointURI h (ApiMethod method) =
  urlPrefix ++ "/bot" ++ apiToken ++ "/" ++ method
  where
    urlPrefix = confURLPrefixWithoutTrailingSlash (hConfig h)
    apiToken = T.unpack . confApiToken $ hConfig h

decodeHttpResult ::
     ApiMethod
  -> (A.Value -> A.Parser response)
  -> HttpResult
  -> Failable response
decodeHttpResult _ _ (Left (HttpError e)) = Left $ IOError e
decodeHttpResult method parser (Right response)
  | inRange (200, 299) status = decodeApiResult parser $ hrsBody response
  | inRange (500, 599) status =
    Left $ HttpStatusError status (hrsStatusText response)
  | otherwise =
    error $ "Method '" ++ apiMethodName method ++ "' returned " ++ show response
  where
    status = hrsStatusCode response

decodeApiResult ::
     (A.Value -> A.Parser response) -> BS.ByteString -> Failable response
decodeApiResult parser body =
  case A.eitherDecode body of
    Left e -> Left $ JSONError e
    Right (ErrorResponse desc) -> Left $ ApiError desc
    Right (ResultResponse value) ->
      either (Left . JSONError) Right $ A.parseEither parser value

instance Logger.Logger (Handle m) m where
  lowLevelLog = Logger.lowLevelLog . hLogHandle

instance Monad m => FlexibleState.Class (Handle m) State m where
  get = FlexibleState.hGet . hStateHandle
  modify' = FlexibleState.hModify' . hStateHandle

parseUpdatesResponse :: A.Value -> A.Parser (Maybe UpdateId, [Event])
parseUpdatesResponse =
  A.withArray "result" $
  fmap ((safeMaximum *** catMaybes) . unzip . catMaybes) .
  mapM (optional . parseUpdate) . toList
  where
    safeMaximum = fmap getMax . foldMap (Just . Max)
    parseUpdate =
      A.withObject "update" $ \update ->
        liftA2 (,) (update .: "update_id") (optional $ parseEvent update)
    parseEvent update =
      (MessageEvent <$> update .: "message") <|>
      (MenuChoiceEvent <$> update .: "callback_query") <|>
      fail "unsupported kind of 'Update' object"

data ApiResponse
  = ErrorResponse Text
  | ResultResponse A.Value

instance A.FromJSON ApiResponse where
  parseJSON =
    A.withObject "response" $ \r -> do
      ok <- r .: "ok"
      if ok
        then ResultResponse <$> r .: "result"
        else ErrorResponse . fromMaybe "" <$> r .:? "description"

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
  deriving (Eq, Show, A.FromJSON, A.ToJSON)

newtype UpdateId =
  UpdateId Int
  deriving (A.FromJSON, A.ToJSON, Enum, Show, Eq, Ord)

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
  ApiMethod
    { apiMethodName :: String
    }
  deriving (Show)

whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust x f = maybe (pure ()) f x
