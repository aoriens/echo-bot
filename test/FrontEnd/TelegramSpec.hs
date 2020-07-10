{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.TelegramSpec
  ( spec
  ) where

import Control.Monad
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BS
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Maybe
import qualified Data.Text as T
import qualified FrontEnd.Telegram as T
import qualified Logger
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Util.FlexibleState as FlexibleState

spec :: Spec
spec =
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
      body ! "timeout" `shouldBe` A.toJSON timeout
    it "should have first getUpdates request with zero offset or no offset" $ do
      e <- newEnv
      let zero = A.toJSON (0 :: Int)
      let h = defaultHandleWithEmptyGetUpdatesResponseStub e
      void $ T.receiveEvents h
      (body:_) <- bodies <$> getRequests e
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
        body ! "offset" `shouldBe` A.toJSON (max id1 id2 + 1)

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

defaultHandleWithEmptyGetUpdatesResponseStub :: Env -> T.Handle IO
defaultHandleWithEmptyGetUpdatesResponseStub env =
  defaultHandleWithHttpHandlers
    env
    [makeResponseForMethod "getUpdates" $ successfulResponse A.emptyArray]

defaultHandleWithHttpHandlers ::
     Env -> [T.HttpRequest -> IO (Maybe A.Value)] -> T.Handle IO
defaultHandleWithHttpHandlers env handlers =
  (defaultHandle env)
    {T.hGetHttpResponse = httpServerStubWithHandlers env handlers}

defaultHandle :: Env -> T.Handle IO
defaultHandle env =
  T.Handle
    { T.hLogHandle = logHandle env
    , T.hGetHttpResponse = error "No hGetHttpResponse specified"
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
-- requests. It has partial type, so that it can return Nothing
-- wrapped in the monad to designate that another handler should be
-- tried to generate a response.
type HttpRequestHandler = T.HttpRequest -> IO (Maybe A.Value)

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
  pure . maybe fatal A.encode $ listToMaybe responses
  where
    fatal = error $ "No response stub provided for request: " ++ show request

type ApiMethod = String

makeResponseForMethod :: ApiMethod -> A.Value -> HttpRequestHandler
makeResponseForMethod method response request =
  pure $
  if uriWithMethod method == T.hrURI request
    then Just response
    else Nothing
