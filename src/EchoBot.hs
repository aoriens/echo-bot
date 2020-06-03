{-# LANGUAGE OverloadedStrings #-}

module EchoBot
  ( respond
  , Request(..)
  , Response(..)
  , ChoiceId
  ) where

import Control.Arrow
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

data Request
  = InMessage Text
  | InMenuChoice ChoiceId

data Response
  = OutText Text
  | OutMenu Text [(Text, ChoiceId)]
  | OutNothing

newtype ChoiceId =
  RepetitionCountChoice Int

respond :: Request -> Response
respond (InMenuChoice (RepetitionCountChoice _)) = OutNothing
respond (InMessage text)
  | isCommand "/help" = OutText "This is usage description"
  | isCommand "/repeat" = OutMenu "Select the number of repetitions" choices
  | otherwise = OutText text
  where
    isCommand cmd = startsWithWord cmd $ T.stripStart text
    choices = map (T.pack . show &&& RepetitionCountChoice) [1 .. 5]

startsWithWord :: Text -> Text -> Bool
startsWithWord word text =
  case T.stripPrefix word text of
    Nothing -> False
    Just rest ->
      case T.uncons rest of
        Nothing -> True
        Just (c, _) -> isSpace c
