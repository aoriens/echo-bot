{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.TelegramSpec
  ( spec
  ) where

import Control.Monad.Writer.Lazy
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as BS
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import qualified FrontEnd.Telegram as T
import qualified Logger
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec =
  describe "FrontEnd.Telegram" $ do
    it "should send getUpdates as the first request" $ do
      let (request:_) = onlyRequests . interp $ T.run defaultHandle
      T.hrURI request `shouldBe` uriWithMethod "getUpdates"
    prop "should send configured timeout in getUpdates" $ \(NonNegative timeout) -> do
      let h =
            defaultHandle
              {T.hConfig = defaultConfig {T.confPollTimeout = timeout}}
          (body:_) = bodies . onlyRequests . interp $ T.run h
      body ! "timeout" `shouldBe` A.toJSON timeout
    it "should have first getUpdates request with zero offset or no offset" $ do
      let zero = A.toJSON (0 :: Int)
          (body:_) = bodies . onlyRequests . interp $ T.run defaultHandle
      HM.lookupDefault zero "offset" body `shouldBe` zero
    it
      "should have getUpdates with offset one more than maximum of last \
      \update_id's received" $
      property $ \(NonNegative id1) (NonNegative id2) -> do
        let _ = (id1 :: Int, id2 :: Int)
            h =
              defaultHandleWithHttpHandlers
                [ makeResponseForMethod "getUpdates" $
                  successfulResponse
                    [ A.object ["update_id" .= id1]
                    , A.object ["update_id" .= id2]
                    ]
                ]
            (_:body:_) =
              bodies . requestsWithMethod "getUpdates" . interp $ T.run h
        body ! "offset" `shouldBe` A.toJSON (max id1 id2 + 1)

-- * Stubs
defaultHandle :: T.Handle Interp
defaultHandle =
  T.Handle
    { T.hBotHandle = error "TODO: no bot handle should be needed"
    , T.hLogHandle = logHandle
    , T.hGetHttpResponse = httpServerStubWithHandlers []
    , T.hStateHandle = undefined
    , T.hConfig = defaultConfig
    }

defaultHandleWithHttpHandlers ::
     [T.HttpRequest -> Interp (Maybe A.Value)] -> T.Handle Interp
defaultHandleWithHttpHandlers handlers =
  defaultHandle {T.hGetHttpResponse = httpServerStubWithHandlers handlers}

defaultConfig :: T.Config
defaultConfig =
  T.Config
    { T.confApiToken = T.pack defaultApiToken
    , T.confURLPrefixWithoutTrailingSlash = defaultURLPrefix
    , T.confPollTimeout = 123
    , T.confConnectionTimeout = 456
    }

defaultApiToken :: String
defaultApiToken = "APITOKEN"

defaultURLPrefix :: String
defaultURLPrefix = "example.com"

logHandle :: Logger.Handle Interp
logHandle =
  Logger.Handle
    { Logger.hLowLevelLog =
        \level _ text ->
          when (level >= Logger.Warning) $ tell [LogEvent level text]
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

-- * The interpreter monad
type Interp = Writer [Event]

data Event
  = HttpRequestEvent T.HttpRequest
  | LogEvent Logger.Level T.Text

interp :: Interp a -> [Event]
interp = execWriter

onlyRequests :: [Event] -> [T.HttpRequest]
onlyRequests = mapMaybe f
  where
    f (HttpRequestEvent r) = Just r
    f _ = Nothing

requestsWithMethod :: String -> [Event] -> [T.HttpRequest]
requestsWithMethod apiMethod = filter p . onlyRequests
  where
    p = (uriWithMethod apiMethod ==) . T.hrURI

-- | A function type for calculating a response for some kind of HTTP
-- requests. It has partial type, so that it can return Nothing
-- wrapped in the monad to designate that another handler should be
-- tried to generate a response.
type HttpRequestHandler = T.HttpRequest -> Interp (Maybe A.Value)

-- | A stub implementation of HTTP requesting.
httpServerStubWithHandlers
     -- | A list of handlers to be run in order until a response body
     -- is returned.
 ::
     [HttpRequestHandler] -> T.HttpRequest -> Interp BS.ByteString
httpServerStubWithHandlers handlers request = do
  tell [HttpRequestEvent request]
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
