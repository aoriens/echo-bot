{-# LANGUAGE OverloadedStrings #-}

module Tests.EchoBot
  ( tests
  ) where

import qualified Control.Monad.State as S
import qualified Data.Text as T
import EchoBot
import qualified Logger
import Test.HUnit hiding (State)

type Interp = S.StateT State IO

tests :: Test
tests =
  test
    [ "The bot should echo a simple comment" ~: do
        let comment = "comment"
        let config = stubConfig
        response <-
          interpret (stateWith config) $
          respond (handleWith config) (ReplyRequest comment)
        RepliesResponse [comment] @=? response
    , "The bot should echo a simple comment for a number of times specified in the config" ~: do
        let comment = "comment"
        let repCount = 3
        let config = stubConfig {confRepetitionCount = repCount}
        response <-
          interpret (stateWith config) $
          respond (handleWith config) (ReplyRequest comment)
        RepliesResponse (replicate repCount comment) @=? response
    ]

interpret :: State -> Interp a -> IO a
interpret s0 m = S.evalStateT m s0

handleWith :: Config -> Handle Interp
handleWith config =
  Handle
    { hGetState = S.get
    , hModifyState = S.modify'
    , hLogHandle = logHandle
    , hConfig = config
    }

logHandle :: (Applicative m) => Logger.Handle m
logHandle = Logger.Handle {Logger.log = \_ _ -> pure ()}

stubConfig :: Config
stubConfig =
  Config
    { confRepeatReply = T.empty
    , confHelpReply = T.empty
    , confRepetitionCount = 1
    }

stateWith :: Config -> State
stateWith = either (error . T.unpack) id . makeState
