{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Telegram.Impl.HTTP
  ( Config(..)
  , new
  ) where

import Control.Arrow
import qualified Data.ByteString.Lazy as BS
import Data.IORef
import Data.Maybe
import Data.String
import qualified EchoBot
import qualified FrontEnd.Telegram as T
import qualified Logger
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.URI as URI
import qualified Util.FlexibleState as FlexibleState

newtype Config =
  Config
    -- | A default, minimum response timeout for all requests.
    { confResponseTimeout :: Int
    }

-- | Creates a @FrontEnd.Telegram.Handle@ using http-client.
new ::
     EchoBot.Handle IO
  -> Logger.Handle IO
  -> Config
  -> T.Config
  -> IO (T.Handle IO)
new botHandle logHandle myConfig telegramConfig = do
  httpManager <- HTTP.newManager TLS.tlsManagerSettings
  stateRef <- newIORef T.makeState
  pure
    T.Handle
      { T.hBotHandle = botHandle
      , T.hLogHandle = logHandle
      , T.hGetHttpResponse = getHttpResponse httpManager myConfig
      , T.hStateHandle =
          FlexibleState.Handle
            { FlexibleState.hGet = readIORef stateRef
            , FlexibleState.hModify' = modifyIORef' stateRef
            }
      , T.hConfig = telegramConfig
      }

getHttpResponse :: HTTP.Manager -> Config -> T.HttpRequest -> IO BS.ByteString
getHttpResponse httpManager config request = do
  httpRequest <- configureRequest <$> HTTP.requestFromURI uri
  HTTP.responseBody <$> HTTP.httpLbs httpRequest httpManager
  where
    configureRequest httpRequest =
      httpRequest
        { HTTP.checkResponse = HTTP.throwErrorStatusCodes
        , HTTP.method = methodString $ T.hrMethod request
        , HTTP.requestHeaders =
            map (fromString *** fromString) $ T.hrHeaders request
        , HTTP.requestBody = HTTP.RequestBodyLBS $ T.hrBody request
        , HTTP.responseTimeout =
            HTTP.responseTimeoutMicro . (1000000 *) $
            T.hrAdditionalResponseTimeout request + confResponseTimeout config
        }
    uri =
      fromMaybe (error $ "Bad URI: " ++ uriString) . URI.parseURI $ uriString
    uriString = T.hrURI request
    methodString T.POST = "POST"
