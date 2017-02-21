module Integration where

import Test.QuickCheck
import Control.Monad.Eff
import Debug.Trace

import Test.Fixture.Test1 as Test1

main = testAll

foreign import consoleLog :: forall a. a -> a

testAll :: QC Unit
testAll = do
  let x = consoleLog (Test1.render "String(x)" { z: "String(y.z)", y: "String(y.y)"} )
  assert true

assert :: Boolean -> QC Unit
assert = quickCheck' 1

