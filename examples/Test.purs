module Test where

import Test.Text.Hatter ()
import Test.Text.Hatter.Parser ()

main = do
  Test.Text.Hatter.testAll
  Test.Text.Hatter.Parser.testAll
