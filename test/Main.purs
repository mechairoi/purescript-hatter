module Test.Main where

import Prelude
import Test.Text.Hatter as H
import Test.Text.Hatter.Parser as HP

main = do
  H.testAll
  HP.testAll
