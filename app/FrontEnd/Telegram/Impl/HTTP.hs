{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Telegram.Impl.HTTP
  ( new
  ) where

import Control.Arrow
import qualified Data.ByteString.Lazy as BS
import Data.IORef
import Data.Maybe
import Data.String
import qualified EchoBot
import FrontEnd.Telegram
import qualified Logger
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.URI as URI

new :: EchoBot.Handle IO -> Logger.Handle IO -> Config -> IO Handle
new botHandle logHandle config = do
  httpManager <- HTTP.newManager TLS.tlsManagerSettings
  stateRef <- newIORef makeState
  pure
    Handle
      { hBotHandle = botHandle
      , hLogHandle = logHandle
      , hGetHttpResponse = getHttpResponse httpManager
      , hGetState = readIORef stateRef
      , hModifyState' = modifyIORef' stateRef
      , hConfig = config
      }

getHttpResponse :: HTTP.Manager -> HttpRequest -> IO BS.ByteString
getHttpResponse httpManager request = do
  httpRequest <- configureRequest <$> HTTP.requestFromURI uri
  HTTP.responseBody <$> HTTP.httpLbs httpRequest httpManager
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
