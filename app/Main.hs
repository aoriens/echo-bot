module Main (main) where

import Control.Monad

main :: IO ()
main = do
  putStrLn "Welcome to the echo-bot"
  forever $ getLine >>= putStrLn
