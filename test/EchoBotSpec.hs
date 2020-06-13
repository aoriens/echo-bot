{-# LANGUAGE OverloadedStrings #-}

module EchoBotSpec
  ( spec
  ) where

import Control.Monad
import qualified Control.Monad.State as S
import Control.Monad.Writer
import qualified Data.Text as T
import EchoBot
import qualified Logger
import Test.Hspec

type Interp = WriterT [(Logger.Level, T.Text)] (S.StateT State IO)

spec :: Spec
spec =
  describe "respond" $ do
    it "should echo a simple comment" $ do
      let comment = "comment"
      let config = stubConfig
      let h = handleWith config
      response <-
        interpret (stateWith config) $ respond h (ReplyRequest comment)
      response `shouldBe` RepliesResponse [comment]
    it
      "should echo a simple comment for a number of times specified in the config" $ do
      let comment = "comment"
      let repCount = 3
      let config = stubConfig {confRepetitionCount = repCount}
      let h = handleWith config
      response <-
        interpret (stateWith config) $ respond h (ReplyRequest comment)
      response `shouldBe` RepliesResponse (replicate repCount comment)
    it "should output menu for /repeat command" $ do
      let config = stubConfig
      let h = handleWith config
      response <-
        interpret (stateWith config) $ respond h (ReplyRequest "/repeat")
      response `shouldSatisfy` isMenuResponse
    it "should honor the repetition count set by the user" $ do
      let comment = "comment"
      let config = stubConfig {confRepetitionCount = 1}
      let newRepCount = 3
      let h = handleWith config
      response <-
        interpret (stateWith config) $ do
          (MenuResponse _ opts) <- respond h $ ReplyRequest "/repeat"
          Just request <- pure $ lookup newRepCount opts
          void $ respond h request
          respond h $ ReplyRequest comment
      response `shouldBe` RepliesResponse (replicate newRepCount comment)

isMenuResponse :: Response -> Bool
isMenuResponse (MenuResponse _ _) = True
isMenuResponse _ = False

interpret :: State -> Interp a -> IO a
interpret s0 m = do
  (a, logMessages) <- S.evalStateT (runWriterT m) s0
  logMessages `shouldSatisfy` null
  pure a

handleWith :: Config -> Handle Interp
handleWith config =
  Handle
    { hGetState = S.get
    , hModifyState = S.modify'
    , hLogHandle = logHandle
    , hConfig = config
    }

logHandle :: Logger.Handle Interp
logHandle =
  Logger.Handle
    { Logger.log =
        \level text -> when (level >= Logger.Warning) $ tell [(level, text)]
    }

stubConfig :: Config
stubConfig =
  Config
    { confRepeatReply = T.empty
    , confHelpReply = T.empty
    , confRepetitionCount = 1
    }

stateWith :: Config -> State
stateWith = either (error . T.unpack) id . makeState
