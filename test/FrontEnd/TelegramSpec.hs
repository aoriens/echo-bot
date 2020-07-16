{-# LANGUAGE OverloadedStrings, TupleSections #-}

module FrontEnd.TelegramSpec
  ( spec
  ) where

import Control.Monad
import qualified Data.Aeson as A
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Maybe
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.Telegram as T
import qualified Logger
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Util.FlexibleState as FlexibleState

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 = do
  describe "receiveEvents" $ do
    it "should send getUpdates as the first request" $ do
      e <- newEnv
      let h = defaultHandleWithEmptyGetUpdatesResponseStub e
      void $ T.receiveEvents h
      (request:_) <- getRequests e
      T.hrURI request `shouldBe` uriWithMethod "getUpdates"
    prop "should send configured timeout in getUpdates" $ \(NonNegative timeout) -> do
      e <- newEnv
      let h =
            (defaultHandleWithEmptyGetUpdatesResponseStub e)
              {T.hConfig = defaultConfig {T.confPollTimeout = timeout}}
      void $ T.receiveEvents h
      (body:_) <- bodies <$> getRequests e
      HM.lookup "timeout" body `shouldBe` Just (A.toJSON timeout)
    it "should have first getUpdates request with zero offset or no offset" $ do
      e <- newEnv
      let zero = A.toJSON (0 :: Int)
      let h = defaultHandleWithEmptyGetUpdatesResponseStub e
      void $ T.receiveEvents h
      (body:_) <- bodies <$> getRequestsWithMethod e "getUpdates"
      HM.lookupDefault zero "offset" body `shouldBe` zero
    it
      "should have getUpdates with offset one more than maximum of last \
      \update_id's received" $
      property $ \(NonNegative id1) (NonNegative id2) -> do
        e <- newEnv
        let _ = (id1 :: Int, id2 :: Int)
            h1 =
              defaultHandleWithHttpHandlers
                e
                [ makeResponseForMethod "getUpdates" $
                  successfulResponse
                    [ A.object ["update_id" .= id1]
                    , A.object ["update_id" .= id2]
                    ]
                ]
            h2 = defaultHandleWithEmptyGetUpdatesResponseStub e
        void $ T.receiveEvents h1
        void $ T.receiveEvents h2
        (_:body:_) <- bodies <$> getRequestsWithMethod e "getUpdates"
        HM.lookup "offset" body `shouldBe` Just (A.toJSON $ max id1 id2 + 1)
    it "should send exactly one getUpdates" $ do
      e <- newEnv
      let h = defaultHandleWithEmptyGetUpdatesResponseStub e
      void $ T.receiveEvents h
      requests <- getRequestsWithMethod e "getUpdates"
      length requests `shouldBe` 1
    it
      "should send the same offset in getUpdates if no entries returned in \
      \last getUpdates response" $
      property $ \(NonNegative updateId) -> do
        e <- newEnv
        let _ = updateId :: Int
            h1 =
              defaultHandleWithHttpHandlers
                e
                [ makeResponseForMethod "getUpdates" $
                  successfulResponse [A.object ["update_id" .= updateId]]
                ]
            h2 = defaultHandleWithEmptyGetUpdatesResponseStub e
            h3 = defaultHandleWithEmptyGetUpdatesResponseStub e
        void $ T.receiveEvents h1
        void $ T.receiveEvents h2
        forgetRequests e
        void $ T.receiveEvents h3
        (body:_) <- bodies <$> getRequestsWithMethod e "getUpdates"
        HM.lookup "offset" body `shouldBe` Just (A.toJSON $ updateId + 1)
    it "should receive as many MessageEvents as user messages arrives" $
      property $ \texts -> do
        e <- newEnv
        let _ = (texts :: [String])
            makeUpdate updateId text =
              A.object
                [ "update_id" .= updateId
                , "message" .=
                  A.object
                    [ "chat" .= A.object ["id" .= (2 :: Int)]
                    , "text" .= T.pack text
                    ]
                ]
            h =
              defaultHandleWithHttpHandlers
                e
                [ makeResponseForMethod "getUpdates" . successfulResponse $
                  zipWith makeUpdate [(1 :: Int) ..] texts
                ]
        events <- T.receiveEvents h
        map snd events `shouldBe` map (EchoBot.MessageEvent . T.pack) texts
    it "should receive chatIds arrived in user messages" $
      property $ \rawChatId -> do
        e <- newEnv
        let expectedChatId = T.ChatId rawChatId
            h =
              defaultHandleWithHttpHandlers
                e
                [ makeResponseForMethod "getUpdates" . successfulResponse $
                  [ A.object
                      [ "update_id" .= (1 :: Int)
                      , "message" .=
                        A.object
                          [ "chat" .= A.object ["id" .= rawChatId]
                          , "text" .= ("text" :: String)
                          ]
                      ]
                  ]
                ]
        (chatId, _) <- head <$> T.receiveEvents h
        chatId `shouldBe` expectedChatId
    it "should ignore malformed updates and return well-formed MessageEvents" $ do
      e <- newEnv
      let h =
            defaultHandleWithHttpHandlers
              e
              [ makeResponseForMethod "getUpdates" . successfulResponse $
                [ entryWithoutId
                , entryWithoutMessage
                , entryWithoutChatId
                , entryWithNonStringText
                , correctEntry
                , entryWithoutText
                ]
              ]
          correctEntry =
            A.object
              [ "update_id" .= (1 :: Int)
              , "message" .=
                A.object
                  [ "chat" .= A.object ["id" .= (1 :: Int)]
                  , "text" .= correctEntryText
                  ]
              ]
          correctEntryText = ("correctText" :: T.Text)
          entryWithoutId =
            A.object
              [ "message" .=
                A.object
                  [ "chat" .= A.object ["id" .= (1 :: Int)]
                  , "text" .= ("text" :: String)
                  ]
              ]
          entryWithoutMessage = A.object ["update_id" .= (2 :: Int)]
          entryWithoutChatId =
            A.object
              [ "update_id" .= (1 :: Int)
              , "message" .=
                A.object ["chat" .= A.object [], "text" .= correctEntryText]
              ]
          entryWithNonStringText =
            A.object
              [ "update_id" .= (1 :: Int)
              , "message" .=
                A.object
                  [ "chat" .= A.object ["id" .= (1 :: Int)]
                  , "text" .= (2222 :: Int)
                  ]
              ]
          entryWithoutText =
            A.object
              [ "update_id" .= (1 :: Int)
              , "message" .= A.object ["chat" .= A.object ["id" .= (1 :: Int)]]
              ]
      events <- T.receiveEvents h
      map snd events `shouldBe` map EchoBot.MessageEvent [correctEntryText]
    it "should send answerCallbackQuery for each CallbackQuery" $
      property $ \testData -> do
        e <- newEnv
        let _ = (testData :: [(String, String, Int, Int, Int)])
            h =
              defaultHandleWithHttpHandlers
                e
                [ makeResponseForMethod "getUpdates" . successfulResponse $
                  map makeCallbackQuery testData
                , makeResponseForMethod "answerCallbackQuery" $
                  successfulResponse True
                ]
            makeCallbackQuery (cqId, cqData, updateId, chatId, messageId) =
              A.object
                [ "update_id" .= updateId
                , "callback_query" .=
                  A.object
                    [ "id" .= cqId
                    , "data" .= cqData
                    , "message" .=
                      A.object
                        [ "message_id" .= messageId
                        , "chat" .= A.object ["id" .= chatId]
                        ]
                    ]
                ]
        void $ T.receiveEvents h
        receivedIds <-
          map (HM.lookup "callback_query_id") . bodies <$>
          getRequestsWithMethod e "answerCallbackQuery"
        receivedIds `shouldMatchList`
          map (\(cqId, _, _, _, _) -> Just $ A.toJSON cqId) testData
  describe "handleBotResponse" $ do
    it
      "should send a message to the correct chat for each entry in \
      \a EchoBot.RepliesResponse" $
      property $ \rawChatId strings -> do
        e <- newEnv
        let chatId = T.ChatId rawChatId
            botResponse = EchoBot.RepliesResponse $ map T.pack strings
            h =
              defaultHandleWithHttpHandlers
                e
                [ makeResponseForMethod "sendMessage" $
                  successfulResponse A.emptyArray
                ]
        T.handleBotResponse h chatId botResponse
        rBodies <- bodies <$> getRequestsWithMethod e "sendMessage"
        map (HM.lookup "text") rBodies `shouldBe` map (Just . A.toJSON) strings
        map (HM.lookup "chat_id") rBodies `shouldSatisfy`
          all (Just (A.toJSON rawChatId) ==)
    it
      "should do sendMessage if previous sendMessage resulted in malformed response" $ do
      e <- newEnv
      let chatId = T.ChatId 1
          botResponse = EchoBot.RepliesResponse ["text1", "text2"]
          h =
            defaultHandleWithHttpHandlers
              e
              [makeRawResponseForMethod "sendMessage" "BAD_JSON!"]
      T.handleBotResponse h chatId botResponse
      requests <- getRequestsWithMethod e "sendMessage"
      length requests `shouldBe` 2
    it
      "should send a message with the menu title and an inline keyboard \
      \for menu response" $ do
      e <- newEnv
      let rawChatId = 123
          chatId = T.ChatId rawChatId
          callbackEvent = error "This event should't be needed"
          title = "Menu title"
          opts = [1, 2]
          h =
            defaultHandleWithHttpHandlers
              e
              [ makeResponseForMethod "sendMessage" $
                successfulResponse A.emptyArray
              ]
          botResponse = EchoBot.MenuResponse title $ map (, callbackEvent) opts
      T.handleBotResponse h chatId botResponse
      (body:_) <- bodies <$> getRequestsWithMethod e "sendMessage"
      let buttonTitles =
            flip A.parseEither body $ \o -> do
              keyboard <- o .: "reply_markup"
              [buttons] <- keyboard .: "inline_keyboard"
              mapM (.: "text") buttons
      HM.lookup "chat_id" body `shouldBe` Just (A.toJSON rawChatId)
      HM.lookup "text" body `shouldBe` Just (A.toJSON title)
      buttonTitles `shouldBe` Right (map show opts)
    it
      "should edit the menu removing buttons and modifying the title \
       \if a new menu is output" $ do
      e <- newEnv
      let rawChatId = 123
          chatId = T.ChatId rawChatId
          title = "Menu title"
          editedMenuTitleSuffix = "(edited)"
          editedTitle = title <> editedMenuTitleSuffix
          menuMessageId = (111 :: Int)
          h =
            (defaultHandle e)
              { T.hGetHttpResponse =
                  httpServerStubWithHandlers
                    e
                    [ makeResponseForMethod "sendMessage" $
                      successfulResponse $
                      A.object ["message_id" .= menuMessageId]
                    , makeResponseForMethod "editMessageText" $
                      successfulResponse A.emptyArray
                    ]
              , T.hConfig =
                  defaultConfig
                    {T.confOutdatedMenuTitleSuffix = editedMenuTitleSuffix}
              }
          menuResponse = EchoBot.MenuResponse title [(1, callbackEvent)]
          callbackEvent = error "No callback event should be needed"
      T.handleBotResponse h chatId menuResponse
      forgetRequests e
      T.handleBotResponse h chatId menuResponse
      (body:_) <- bodies <$> getRequestsWithMethod e "editMessageText"
      HM.lookup "message_id" body `shouldBe` Just (A.toJSON menuMessageId)
      HM.lookup "chat_id" body `shouldBe` Just (A.toJSON rawChatId)
      HM.lookup "text" body `shouldBe` Just (A.toJSON editedTitle)
      HM.lookup "reply_markup" body `shouldBe` Nothing

data Env =
  Env
    { eState :: IORef T.State
    , eReverseRequests :: IORef [T.HttpRequest]
    , eReverseLogEntries :: IORef [(Logger.Level, T.Text)]
    }

newEnv :: IO Env
newEnv = do
  state <- newIORef T.makeState
  requests <- newIORef []
  logEntries <- newIORef []
  pure
    Env
      { eState = state
      , eReverseRequests = requests
      , eReverseLogEntries = logEntries
      }

forgetRequests :: Env -> IO ()
forgetRequests env = writeIORef (eReverseRequests env) []

defaultHandleWithEmptyGetUpdatesResponseStub :: Env -> T.Handle IO
defaultHandleWithEmptyGetUpdatesResponseStub env =
  defaultHandleWithHttpHandlers
    env
    [makeResponseForMethod "getUpdates" $ successfulResponse A.emptyArray]

defaultHandleWithHttpHandlers ::
     Env -> [T.HttpRequest -> IO (Maybe BS.ByteString)] -> T.Handle IO
defaultHandleWithHttpHandlers env handlers =
  (defaultHandle env)
    {T.hGetHttpResponse = httpServerStubWithHandlers env handlers}

defaultHandle :: Env -> T.Handle IO
defaultHandle env =
  T.Handle
    { T.hLogHandle = logHandle env
    , T.hGetHttpResponse =
        \request ->
          pure $ error ("No response provided for request: " ++ show request)
    , T.hStateHandle =
        FlexibleState.Handle
          { FlexibleState.hGet = readIORef $ eState env
          , FlexibleState.hModify' = modifyIORef' $ eState env
          }
    , T.hConfig = defaultConfig
    }

defaultConfig :: T.Config
defaultConfig =
  T.Config
    { T.confApiToken = T.pack defaultApiToken
    , T.confURLPrefixWithoutTrailingSlash = defaultURLPrefix
    , T.confPollTimeout = 123
    , T.confOutdatedMenuTitleSuffix = ""
    }

defaultApiToken :: String
defaultApiToken = "APITOKEN"

defaultURLPrefix :: String
defaultURLPrefix = "example.com"

logHandle :: Env -> Logger.Handle IO
logHandle env =
  Logger.Handle
    { Logger.hLowLevelLog =
        \level _ text ->
          when (level >= Logger.Warning) $
          modifyIORef' (eReverseLogEntries env) ((level, text) :)
    }

uriWithMethod :: String -> String
uriWithMethod method =
  defaultURLPrefix ++ "/bot" ++ defaultApiToken ++ "/" ++ method

decodeJsonObject :: BS.ByteString -> A.Object
decodeJsonObject bs = either error id $ A.eitherDecode bs

bodies :: [T.HttpRequest] -> [A.Object]
bodies = map $ decodeJsonObject . T.hrBody

successfulResponse :: (A.ToJSON a) => a -> A.Value
successfulResponse payload = A.object ["result" .= payload]

getRequests :: Env -> IO [T.HttpRequest]
getRequests env = reverse <$> readIORef (eReverseRequests env)

getRequestsWithMethod :: Env -> String -> IO [T.HttpRequest]
getRequestsWithMethod env apiMethod = filter p <$> getRequests env
  where
    p = (uriWithMethod apiMethod ==) . T.hrURI

-- | A function type for calculating a response for some kind of HTTP
-- requests. It can return Nothing wrapped in the monad to designate
-- that another handler should be tried to generate a response.
type HttpRequestHandler = T.HttpRequest -> IO (Maybe BS.ByteString)

-- | A stub implementation of HTTP requesting.
httpServerStubWithHandlers ::
     Env
     -- | A list of handlers to be run in order until a response body
     -- is returned.
  -> [HttpRequestHandler]
  -> T.HttpRequest
  -> IO BS.ByteString
httpServerStubWithHandlers env handlers request = do
  modifyIORef' (eReverseRequests env) (request :)
  responses <- catMaybes <$> mapM ($ request) handlers
  pure . fromMaybe fatal $ listToMaybe responses
  where
    fatal = error $ "No response stub provided for request: " ++ show request

type ApiMethod = String

makeResponseForMethod :: ApiMethod -> A.Value -> HttpRequestHandler
makeResponseForMethod method = makeRawResponseForMethod method . A.encode

makeRawResponseForMethod :: ApiMethod -> BS.ByteString -> HttpRequestHandler
makeRawResponseForMethod method body request =
  pure $
  if uriWithMethod method == T.hrURI request
    then Just body
    else Nothing
