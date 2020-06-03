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

class Monad m =>
      Gateway m
  where
  get :: m BotState
  put :: BotState -> m ()

data Request
  = InMessage Text
  | InMenuChoice ChoiceId

data Response
  = OutTexts [Text]
  | OutMenu Text [(Text, ChoiceId)]
  | OutNothing

newtype ChoiceId =
  RepetitionCountChoice Int

newtype BotState =
  BotState
    { stRepetitionCount :: Int
    }

makeState :: BotState
makeState = BotState {stRepetitionCount = 1}

respond :: (Gateway m) => Request -> m Response
respond (InMenuChoice (RepetitionCountChoice repetitionCount)) =
  handleSettingRepetitionCount repetitionCount
respond (InMessage text)
  | isCommand "/help" = pure $ OutTexts ["This is usage description"]
  | isCommand "/repeat" = handleRepeatCommand
  | otherwise = respondWithEchoedComment text
  where
    isCommand cmd = startsWithWord cmd $ T.stripStart text

handleSettingRepetitionCount :: (Gateway m) => Int -> m Response
handleSettingRepetitionCount count = do
  s <- get
  put s {stRepetitionCount = count}
  pure OutNothing

handleRepeatCommand :: (Gateway m) => m Response
handleRepeatCommand = do
  count <- stRepetitionCount <$> get
  pure $ OutMenu (title count) choices
  where
    title count =
      "The current repetition amount is " `T.append` T.pack (show count) `T.append`
      "\nSelect the number of repetitions"
    choices = map (T.pack . show &&& RepetitionCountChoice) [1 .. 5]

respondWithEchoedComment :: (Gateway m) => Text -> m Response
respondWithEchoedComment comment = do
  count <- stRepetitionCount <$> get
  pure . OutTexts . replicate count $ comment

startsWithWord :: Text -> Text -> Bool
startsWithWord word text =
  case T.stripPrefix word text of
    Nothing -> False
    Just rest ->
      case T.uncons rest of
        Nothing -> True
        Just (c, _) -> isSpace c
