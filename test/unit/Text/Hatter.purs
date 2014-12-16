module UnitTest.Text.Hatter where

import Test.QuickCheck
import Text.Hatter.Parser
import Control.Monad.Eff
import Debug.Trace
import Data.Either

import Text.Hatter (hatter)

input :: String
input = """render :: String -> String -> VTree
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

testAll :: QC Unit
testAll = do
  assert $ isRight $ hatter "Test.Text.Hatter.Case1" [] input

assert :: Boolean -> QC Unit
assert = quickCheck' 1
