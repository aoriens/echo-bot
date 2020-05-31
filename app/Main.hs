{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad
import Data.Text.IO as TIO

main :: IO ()
main = do
  TIO.putStrLn "Welcome to the echo-bot"
  forever $ TIO.getLine >>= TIO.putStrLn
