{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.TelegramSpec
  ( spec
  ) where

import qualified FrontEnd.Telegram as T
import Data.Maybe
import Test.Hspec
import qualified Logger
import Control.Monad.Writer.Lazy
import qualified Data.Text as T
import Data.ByteString.Lazy as BS

spec :: Spec
spec =
  describe "FrontEnd.Telegram" $ do
    it "should send getUpdates as the first request" $ do
      let h = makeHandle
      let (r:_) = onlyRequests . interp $ T.run h 
      T.hrURI r `shouldBe` uriWithMethod "getUpdates"

-- * Stubs

makeHandle :: T.Handle Interp
makeHandle =
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
        \level _ text -> when (level >= Logger.Warning) $ tell [LogEvent level text]
    }

uriWithMethod :: String -> String
uriWithMethod method = host ++ "/bot" ++ apiToken ++ "/" ++ method
  where
    host = T.confURLPrefixWithoutTrailingSlash config
    apiToken = T.unpack $ T.confApiToken config
    config = defaultConfig

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
