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
import Test.QuickCheck
import qualified Util.FlexibleState as FS

type Interp = WriterT [(Logger.Level, T.Text)] (S.StateT State IO)

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "respond" $ do
    it "should echo any non-command input back" $
      property $ \str ->
        not (hasCommandPrefix str) ==> do
          let comment = T.pack str
              config = stubConfig
              h = handleWith config
          responses <-
            runBotWithConfig config $ respond h (MessageEvent comment)
          responses `shouldBe` [MessageResponse comment]
    it
      "should echo a simple message for any specified \
      \amount of times specified in the config" $
      property $ \(NonNegative count) -> do
        let comment = "comment"
            config = stubConfig {confRepetitionCount = count}
            h = handleWith config
        responses <- runBotWithConfig config $ respond h (MessageEvent comment)
        responses `shouldBe` replicate count (MessageResponse comment)
    it "should output menu for /repeat command" $ do
      let config = stubConfig
      let h = handleWith config
      responses <- runBotWithConfig config $ respond h (MessageEvent "/repeat")
      responses `shouldSatisfy` any isMenuResponse
    it "should honor the repetition count set by the user" $ do
      let comment = "comment"
          config = stubConfig {confRepetitionCount = 1}
          newRepCount = 3
          h = handleWith config
      responses <-
        runBotWithConfig config $ do
          [MenuResponse _ opts] <- respond h $ MessageEvent "/repeat"
          Just request <- pure $ lookup newRepCount opts
          void $ respond h request
          respond h $ MessageEvent comment
      responses `shouldBe` replicate newRepCount (MessageResponse comment)
    it "should output the help text for /help command" $ do
      let helpText = "My help text"
      let config = stubConfig {confHelpReply = helpText}
      let h = handleWith config
      responses <- runBotWithConfig config $ respond h $ MessageEvent "/help"
      responses `shouldBe` [MessageResponse helpText]
    it "should output the predefined menu title for /repeat command" $ do
      let title = "My title"
      let config = stubConfig {confRepeatReply = title}
      let h = handleWith config
      responses <- runBotWithConfig config $ respond h $ MessageEvent "/repeat"
      responses `shouldSatisfy` any (isMenuResponseWithTitle title)
    it "should substitute {count} with repetition count in the menu title" $ do
      let config =
            stubConfig
              { confRepeatReply = "My count is {count}, {right}."
              , confRepetitionCount = 3
              }
      let h = handleWith config
      responses <- runBotWithConfig config $ respond h $ MessageEvent "/repeat"
      responses `shouldSatisfy`
        any (isMenuResponseWithTitle "My count is 3, {right}.")
    it "should parse a command with trailing and leading spaces" $ do
      shouldRecognizeHelpCommand "/help "
      shouldRecognizeHelpCommand "  /help "
      shouldRecognizeHelpCommand "  /help"
    it "should not recognize an unknown command" $ do
      shouldNotRecognizeHelpCommand "/xhelp"
      shouldNotRecognizeHelpCommand "/ help"
      shouldNotRecognizeHelpCommand "/helpx"
      shouldNotRecognizeHelpCommand "/he lp"
      shouldNotRecognizeHelpCommand "x/help"
      shouldNotRecognizeHelpCommand "x /help"

isMenuResponse :: Response -> Bool
isMenuResponse (MenuResponse _ _) = True
isMenuResponse _ = False

isMenuResponseWithTitle :: T.Text -> Response -> Bool
isMenuResponseWithTitle title (MenuResponse t _) = title == t
isMenuResponseWithTitle _ _ = False

shouldRecognizeHelpCommand :: T.Text -> Expectation
shouldRecognizeHelpCommand = shouldRecognizeHelpCommandOrNot True

shouldNotRecognizeHelpCommand :: T.Text -> Expectation
shouldNotRecognizeHelpCommand = shouldRecognizeHelpCommandOrNot False

shouldRecognizeHelpCommandOrNot :: Bool -> T.Text -> Expectation
shouldRecognizeHelpCommandOrNot matchOrNot input = do
  let helpText = "Help text, not " <> input
  let config = stubConfig {confHelpReply = helpText}
  let h = handleWith config
  response <- runBotWithConfig config $ respond h $ MessageEvent input
  let expected = [MessageResponse helpText]
  if matchOrNot
    then response `shouldBe` expected
    else response `shouldNotBe` expected

hasCommandPrefix :: String -> Bool
hasCommandPrefix (' ':xs) = hasCommandPrefix xs
hasCommandPrefix ('/':_) = True
hasCommandPrefix _ = False

runBotWithConfig :: Config -> Interp a -> IO a
runBotWithConfig config = runBot (stateWith config)

runBot :: State -> Interp a -> IO a
runBot s0 m = do
  (a, logMessages) <- S.evalStateT (runWriterT m) s0
  logMessages `shouldSatisfy` null
  pure a

handleWith :: Config -> Handle Interp
handleWith config =
  Handle
    { hStateHandle = FS.Handle {FS.hGet = S.get, FS.hModify' = S.modify'}
    , hLogHandle = logHandle
    , hConfig = config
    }

logHandle :: Logger.Handle Interp
logHandle =
  Logger.Handle
    { Logger.hLowLevelLog =
        \level _ text -> when (level >= Logger.Warning) $ tell [(level, text)]
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
