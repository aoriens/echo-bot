{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad
import Data.Char
import Data.Text as T
import Data.Text.IO as TIO
import System.IO

main :: IO ()
main = do
  TIO.putStrLn "Welcome to the echo-bot"
  forever $ do
    TIO.putStr "> "
    hFlush stdout
    TIO.putStrLn . responseForRequest . decodeRequest =<< TIO.getLine

data Request
  = Comment Text
  | HelpCommand

responseForRequest :: Request -> Text
responseForRequest (Comment text) = text
responseForRequest HelpCommand = "This is usage description"

decodeRequest :: Text -> Request
decodeRequest text
  | isCommand "/help" = HelpCommand
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
