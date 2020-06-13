module Main
  ( main
  ) where

import Control.Monad
import Test.HUnit
import qualified EchoBotSpec

main :: IO ()
main = void $ runTestTT tests

tests :: Test
tests = EchoBotSpec.tests
