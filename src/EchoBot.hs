{-# LANGUAGE OverloadedStrings, FlexibleInstances,
  MultiParamTypeClasses #-}

module EchoBot
  ( makeState
  , respond
  , Event(MessageEvent)
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
import Logger ((.<))
import qualified Util.FlexibleState as FlexibleState
import Util.FlexibleState (get, modify')

-- | The bot dependencies to be satisfied by the caller.
data Handle m =
  Handle
    { hStateHandle :: FlexibleState.Handle State m
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
data Event
  -- | A text comment
  = MessageEvent Text
  -- | Set the repetition count. The constructor is not considered to
  -- be exported, so that clients could not use it to create requests,
  -- but values based on it could be returned from the module.
  | SetRepetitionCountEvent Int
  deriving (Eq, Show)

-- | Bot reaction to a request.
data Response
  -- | A command to output a comment for the user. Each comment is to
  -- be output as a separate message.
  = MessageResponse Text
  -- | A command to output a menu with the given title and options.
  -- Each option is paired with the corresponding request to perform
  -- on selection.
  | MenuResponse Text [(Int, Event)]
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
respond :: (Monad m) => Handle m -> Event -> m [Response]
respond h (SetRepetitionCountEvent repetitionCount) =
  handleSettingRepetitionCount h repetitionCount
respond h (MessageEvent text)
  | isCommand "/help" = handleHelpCommand h
  | isCommand "/repeat" = handleRepeatCommand h
  | otherwise = respondWithEchoedComment h text
  where
    isCommand cmd = startsWithWord cmd $ T.stripStart text

handleHelpCommand :: (Monad m) => Handle m -> m [Response]
handleHelpCommand h = do
  Logger.info h "Got help command"
  pure [MessageResponse . confHelpReply $ hConfig h]

handleSettingRepetitionCount :: (Monad m) => Handle m -> Int -> m [Response]
handleSettingRepetitionCount h count = do
  Logger.info h $ "User set repetition count to " .< count
  when (count < minRepetitionCount || count > maxRepetitionCount) $ do
    Logger.warn h $
      "Suspicious new repetition count to be set, too little or large: " .<
      count
  modify' (hStateHandle h) $ \s -> s {stRepetitionCount = count}
  pure []

handleRepeatCommand :: (Monad m) => Handle m -> m [Response]
handleRepeatCommand h = do
  Logger.info h "Got repeat command"
  title <- repeatCommandReply h
  pure [MenuResponse title choices]
  where
    choices =
      map
        (id &&& SetRepetitionCountEvent)
        [minRepetitionCount .. maxRepetitionCount]

repeatCommandReply :: (Monad m) => Handle m -> m Text
repeatCommandReply h = do
  count <- stRepetitionCount <$> get (hStateHandle h)
  let countText = T.pack $ show count
      template = confRepeatReply $ hConfig h
  pure $ T.replace "{count}" countText template

minRepetitionCount, maxRepetitionCount :: Int
minRepetitionCount = 1

maxRepetitionCount = 5

respondWithEchoedComment :: (Monad m) => Handle m -> Text -> m [Response]
respondWithEchoedComment h comment = do
  Logger.info h $ "Echoing user input: '" <> comment <> "'"
  count <- stRepetitionCount <$> get (hStateHandle h)
  Logger.debug h $ "Current repetition count is " .< count
  pure . replicate count $ MessageResponse comment

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
  lowLevelLog = Logger.lowLevelLog . hLogHandle

instance Monad m => FlexibleState.Class (Handle m) State m where
  get = FlexibleState.hGet . hStateHandle
  modify' = FlexibleState.hModify' . hStateHandle
