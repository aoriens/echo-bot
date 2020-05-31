{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO
import System.IO

main :: IO ()
main = do
  TIO.putStrLn "Welcome to the echo-bot"
  forever $ do
    TIO.putStr "> "
    hFlush stdout
    TIO.putStrLn . handleInput =<< TIO.getLine
  where
    handleInput = encodeResponse . responseForRequest . decodeRequest

data Request
  = Comment Text
  | HelpCommand
  | RepeatCommand

data Response
  = TextResponse Text
  | MenuResponse Text [Text]

encodeResponse :: Response -> Text
encodeResponse (TextResponse text) = text
encodeResponse (MenuResponse text options) =
  text `T.snoc` '\n' `T.append` formattedOptions
  where
    formattedOptions = T.intercalate "\n" $ map formatOption options
    formatOption opt = "  - " `T.append` opt

responseForRequest :: Request -> Response
responseForRequest (Comment text) = TextResponse text
responseForRequest HelpCommand = TextResponse "This is usage description"
responseForRequest RepeatCommand =
  MenuResponse "Select the number of repetitions" choices
  where
    choices = map (T.pack . show) ([1 .. 5] :: [Int])

decodeRequest :: Text -> Request
decodeRequest text
  | isCommand "/help" = HelpCommand
  | isCommand "/repeat" = RepeatCommand
  | otherwise = Comment text
  where
    isCommand cmd = startsWithWord cmd $ T.stripStart text

startsWithWord :: Text -> Text -> Bool
startsWithWord word text =
  case T.stripPrefix word text of
    Nothing -> False
    Just rest ->
      case T.uncons rest of
        Nothing -> True
        Just (c, _) -> isSpace c
