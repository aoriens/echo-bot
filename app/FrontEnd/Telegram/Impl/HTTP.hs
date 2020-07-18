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
import qualified FrontEnd.Telegram as T
import qualified Logger
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.URI as URI
import qualified Util.FlexibleState as FlexibleState

newtype Config =
  Config
    -- | A default, minimum response timeout for all requests.
    { confResponseTimeout :: Int
    }

-- | Creates a @FrontEnd.Telegram.Handle@ using http-client.
new :: Logger.Handle IO -> Config -> T.Config -> IO (T.Handle IO)
new logHandle myConfig telegramConfig = do
  httpManager <- HTTP.newManager TLS.tlsManagerSettings
  stateRef <- newIORef T.makeState
  pure
    T.Handle
      { T.hLogHandle = logHandle
      , T.hGetHttpResponse = getHttpResponse httpManager myConfig
      , T.hStateHandle =
          FlexibleState.Handle
            { FlexibleState.hGet = readIORef stateRef
            , FlexibleState.hModify' = modifyIORef' stateRef
            }
      , T.hConfig = telegramConfig
      }

getHttpResponse :: HTTP.Manager -> Config -> T.HttpRequest -> IO T.HttpResponse
getHttpResponse httpManager config request = do
  httpRequest <- configureRequest <$> HTTP.requestFromURI uri
  response <- HTTP.httpLbs httpRequest httpManager
  pure
    T.HttpResponse
      { T.hrsStatusCode = HTTP.statusCode $ HTTP.responseStatus response
      , T.hrsStatusText =
          BS.fromStrict . HTTP.statusMessage $ HTTP.responseStatus response
      , T.hrsBody = HTTP.responseBody response
      }
  where
    configureRequest httpRequest =
      httpRequest
        { HTTP.method = methodString $ T.hrMethod request
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
