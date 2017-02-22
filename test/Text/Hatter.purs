module Test.Text.Hatter where

import Prelude
import Test.QuickCheck
import Text.Hatter.Parser
import Control.Monad.Eff
import Debug.Trace
import Data.Either

import Text.Hatter (hatter)

input :: String
input = """
module UnitTest.Text.Hatter.Sample where
render :: String -> String -> VTree
render x y =
  <div>
    <span><% x %> foo</span>
    <% y.z %>
    &amp;&gt;&lt;
    <textarea><script></script></textarea>
    <hr>
    <input type="hidden" name=bar value='ba<% y.y %>z' />
  </div>
"""

testAll :: forall eff. QC eff Unit
testAll = do
  traceAnyM $ "Test.Text.Hatter: "
  -- traceAnyM $ "result: "
  -- traceAnyM $ hatter [] input
  assert $ isRight $ hatter [] input

assert :: forall eff. Boolean -> QC eff Unit
assert = quickCheck' 1
