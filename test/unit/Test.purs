module UnitTest where

import UnitTest.Text.Hatter ()
import UnitTest.Text.Hatter.Parser ()

main = do
  UnitTest.Text.Hatter.testAll
  UnitTest.Text.Hatter.Parser.testAll
