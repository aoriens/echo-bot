module Main (main) where

import Control.Monad
import Lib

main :: IO ()
main = do
  putStrLn "Welcome to the echo-bot"
  forever $ getLine >>= putStrLn
