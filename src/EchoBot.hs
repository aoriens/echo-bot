{-# LANGUAGE OverloadedStrings #-}

module EchoBot
  ( makeState
  , respond
  , Request(..)
  , Response(..)
  , ChoiceId
  , BotState
  , Gateway(..)
  ) where

import Control.Arrow
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

-- | The bot gateway: dependencies it needs to be satisfied from the
-- outside world. Currently, it merely describes state manipulation
-- methods that can easily be implemented without type classes and
-- monads at all, but in such a case the client code would have to be
-- bothered to deal with the state explicitly, so I don't think the
-- monadic type class is such a bad idea.
class Monad m =>
      Gateway m
  where
  get :: m BotState
  put :: BotState -> m ()

-- | An action taken by the user that the bot should respond.
data Request
  -- | A text comment
  = ReplyRequest Text
  -- | A choice has been taken in a previously output menu
  | MenuChoiceRequest ChoiceId

-- | Bot reaction to a request.
data Response
  -- | A command to output several text comments for the user. Each
  -- element in the list is to be output as a separate message.
  = RepliesResponse [Text]
  -- | A command to output a menu with the given title and options.
  -- Each option is paired with the corresponding choice identifier.
  | MenuResponse Text [(Text, ChoiceId)]
  | EmptyResponse

-- | An opaque type to identify available options in a menu for
-- selection.
newtype ChoiceId
  -- | The repetition count identifier, that is used in repetition
  -- count selection menu. It wraps the repetition count.
         =
  RepetitionCountChoice Int

-- | An intermediate state of the bot.
newtype BotState =
  BotState
    { stRepetitionCount :: Int
    }

-- | Creates an initial, default bot state. It can be used when no
-- configuration was loaded.
makeState :: BotState
makeState = BotState {stRepetitionCount = 1}

-- | Evaluates a response for the passed request.
respond :: (Gateway m) => Request -> m Response
respond (MenuChoiceRequest (RepetitionCountChoice repetitionCount)) =
  handleSettingRepetitionCount repetitionCount
respond (ReplyRequest text)
  | isCommand "/help" = pure $ RepliesResponse ["This is usage description"]
  | isCommand "/repeat" = handleRepeatCommand
  | otherwise = respondWithEchoedComment text
  where
    isCommand cmd = startsWithWord cmd $ T.stripStart text

handleSettingRepetitionCount :: (Gateway m) => Int -> m Response
handleSettingRepetitionCount count = do
  s <- get
  put s {stRepetitionCount = count}
  pure EmptyResponse

handleRepeatCommand :: (Gateway m) => m Response
handleRepeatCommand = do
  count <- stRepetitionCount <$> get
  pure $ MenuResponse (makeTitle count) choices
  where
    makeTitle count =
      "The current repetition amount is " <>
      T.pack (show count) <> "\nSelect the number of repetitions"
    choices = map (T.pack . show &&& RepetitionCountChoice) [1 .. 5]

respondWithEchoedComment :: (Gateway m) => Text -> m Response
respondWithEchoedComment comment = do
  count <- stRepetitionCount <$> get
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
