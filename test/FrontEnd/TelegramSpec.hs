{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.TelegramSpec
  ( spec
  ) where

import Control.Monad.Writer.Lazy
import qualified Data.Aeson as A
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
          (request:_) = onlyRequests . interp $ T.run h
          body = decodeJsonObject $ T.hrBody request
      body ! "timeout" `shouldBe` A.toJSON timeout
    it "should have first getUpdates request with zero offset or no offset" $ do
      let zero = A.toJSON (0 :: Int)
          (request:_) = onlyRequests . interp $ T.run defaultHandle
          body = decodeJsonObject $ T.hrBody request
      HM.lookupDefault zero "offset" body `shouldBe` zero

-- * Stubs
defaultHandle :: T.Handle Interp
defaultHandle =
  T.Handle
    { T.hBotHandle = error "TODO: no bot handle should be needed"
    , T.hLogHandle = logHandle
    , T.hGetHttpResponse = getHttpResponse
    , T.hStateHandle = undefined
    , T.hConfig = defaultConfig
    }

defaultConfig :: T.Config
defaultConfig =
  T.Config
    { T.confApiToken = "APITOKEN"
    , T.confURLPrefixWithoutTrailingSlash = "example.com"
    , T.confPollTimeout = 123
    , T.confConnectionTimeout = 456
    }

logHandle :: Logger.Handle Interp
logHandle =
  Logger.Handle
    { Logger.hLowLevelLog =
        \level _ text ->
          when (level >= Logger.Warning) $ tell [LogEvent level text]
    }

uriWithMethod :: String -> String
uriWithMethod method = host ++ "/bot" ++ apiToken ++ "/" ++ method
  where
    host = T.confURLPrefixWithoutTrailingSlash config
    apiToken = T.unpack $ T.confApiToken config
    config = defaultConfig

decodeJsonObject :: BS.ByteString -> A.Object
decodeJsonObject bs = either error id $ A.eitherDecode bs

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

getHttpResponse :: T.HttpRequest -> Interp BS.ByteString
getHttpResponse request = do
  tell [HttpRequestEvent request]
  pure ""
