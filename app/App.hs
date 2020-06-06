{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App
  ( App
  , AppState(..)
  , runApp
  ) where

import Control.Monad.State
import qualified EchoBot

-- | The root monad of the application. It can implement monadic
-- dependencies needed by other high-level modules, and IO.
newtype App a =
  App (StateT AppState IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

newtype AppState =
  AppState
    { botState :: EchoBot.BotState
    }

runApp :: App a -> AppState -> IO a
runApp (App anApp) = evalStateT anApp

instance EchoBot.Gateway App where
  getState = App $ gets botState
  modifyState' f = App . modify' $ \s -> s {botState = f $! botState $ s}
