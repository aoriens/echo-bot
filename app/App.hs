{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App
  ( App
  , Env(..)
  , runApp
  ) where

import Control.Monad.Reader
import Data.IORef
import qualified EchoBot

-- | The root monad of the application. It can implement monadic
-- dependencies needed by other high-level modules, and IO.
newtype App a =
  App (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

newtype Env =
  Env
    { envBotState :: IORef EchoBot.BotState
    }

runApp :: App a -> Env -> IO a
runApp (App anApp) = runReaderT anApp

instance EchoBot.Gateway App where
  getState = App $ asks envBotState >>= liftIO . readIORef
  modifyState' f =
    App $ do
      var <- asks envBotState
      liftIO $ modifyIORef' var f
