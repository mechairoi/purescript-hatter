module Test.Main where

import Prelude
import Test.Text.Hatter as H
import Test.Text.Hatter.Parser as HP
import Test.Integration as I

main = do
  HP.testAll
  H.testAll
  I.testAll
