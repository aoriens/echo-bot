module Main
  ( main
  ) where

import Control.Monad
import Test.HUnit
import qualified Tests.EchoBot

main :: IO ()
main = void $ runTestTT tests

tests :: Test
tests = Tests.EchoBot.tests
