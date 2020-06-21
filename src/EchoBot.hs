{-# LANGUAGE OverloadedStrings, FlexibleInstances,
  MultiParamTypeClasses #-}

module EchoBot
  ( makeState
  , respond
  , Request(ReplyRequest)
  , Response(..)
  , State
  , Handle(..)
  , Config(..)
  ) where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Logger

-- | The bot dependencies to be satisfied by the caller.
data Handle m =
  Handle
    { hGetState :: m State
    , hModifyState :: (State -> State) -> m ()
    , hLogHandle :: Logger.Handle m
    , hConfig :: Config
    }

-- | The initial configuration of the bot.
data Config =
  Config
      -- | A reply to "help" command
    { confHelpReply :: Text
      -- | A reply to "repeat" command. Use @{count}@ as a placeholder
      -- for the current repetition count.
    , confRepeatReply :: Text
      -- | The initial repetition count for echoing messages to start
      -- with.
    , confRepetitionCount :: Int
    }

-- | An action taken by the user that the bot should respond.
data Request
  -- | A text comment
  = ReplyRequest Text
  -- | Set the repetition count. The constructor is not considered to
  -- be exported, so that clients could not use it to create requests,
  -- but values based on it could be returned from the module.
  | SetRepetitionCountRequest Int
  deriving (Eq, Show)

-- | Bot reaction to a request.
data Response
  -- | A command to output several text comments for the user. Each
  -- element in the list is to be output as a separate message.
  = RepliesResponse [Text]
  -- | A command to output a menu with the given title and options.
  -- Each option is paired with the corresponding request to perform
  -- on selection.
  | MenuResponse Text [(Int, Request)]
  | EmptyResponse
  deriving (Eq, Show)

-- | An intermediate state of the bot.
newtype State =
  State
    { stRepetitionCount :: Int
    }

-- | Creates an initial, default bot state.
makeState :: Config -> Either Text State
makeState conf = do
  checkConfig conf
  pure State {stRepetitionCount = confRepetitionCount conf}

checkConfig :: Config -> Either Text ()
checkConfig conf =
  if confRepetitionCount conf < 0
    then Left "The repetition count must not be negative"
    else Right ()

-- | Evaluates a response for the passed request.
respond :: (Monad m) => Handle m -> Request -> m Response
respond h (SetRepetitionCountRequest repetitionCount) =
  handleSettingRepetitionCount h repetitionCount
respond h (ReplyRequest text)
  | isCommand "/help" = handleHelpCommand h
  | isCommand "/repeat" = handleRepeatCommand h
  | otherwise = respondWithEchoedComment h text
  where
    isCommand cmd = startsWithWord cmd $ T.stripStart text

handleHelpCommand :: (Monad m) => Handle m -> m Response
handleHelpCommand h = do
  Logger.info h "Got help command"
  pure $ RepliesResponse [confHelpReply . hConfig $ h]

handleSettingRepetitionCount :: (Monad m) => Handle m -> Int -> m Response
handleSettingRepetitionCount h count = do
  Logger.info h $ "User set repetition count to " <> T.pack (show count)
  when (count < minRepetitionCount || count > maxRepetitionCount) $ do
    Logger.warn h $
      "Suspicious new repetition count to be set, too little or large: " <>
      T.pack (show count)
  hModifyState h $ \s -> s {stRepetitionCount = count}
  pure EmptyResponse

handleRepeatCommand :: (Monad m) => Handle m -> m Response
handleRepeatCommand h = do
  Logger.info h "Got repeat command"
  title <- repeatCommandReply h
  pure $ MenuResponse title choices
  where
    choices =
      map
        (id &&& SetRepetitionCountRequest)
        [minRepetitionCount .. maxRepetitionCount]

repeatCommandReply :: (Monad m) => Handle m -> m Text
repeatCommandReply h = do
  count <- stRepetitionCount <$> hGetState h
  let countText = T.pack $ show count
      template = confRepeatReply $ hConfig h
  pure $ T.replace "{count}" countText template

minRepetitionCount, maxRepetitionCount :: Int
minRepetitionCount = 1

maxRepetitionCount = 5

respondWithEchoedComment :: (Monad m) => Handle m -> Text -> m Response
respondWithEchoedComment h comment = do
  Logger.info h $ "Echoing user input: '" <> comment <> "'"
  count <- stRepetitionCount <$> hGetState h
  Logger.debug h $ "Current repetition count is " <> T.pack (show count)
  pure . RepliesResponse . replicate count $ comment

-- | Determines whether the text starts with a given word. A word is
-- considered as a substring with a trailing whitespace after it or
-- the end of line.
startsWithWord :: Text -> Text -> Bool
startsWithWord word text =
  case T.stripPrefix word text of
    Nothing -> False
    Just rest ->
      case T.uncons rest of
        Nothing -> True
        Just (c, _) -> isSpace c

instance Logger.Logger (Handle m) m where
  log = Logger.log . hLogHandle
