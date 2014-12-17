module IntegrationTest where

import Test.QuickCheck
import Control.Monad.Eff
import Debug.Trace
import Test1 ()

main = testAll

foreign import consoleLog "function consoleLog(x) { console.log(JSON.stringify(x)); return x }" :: forall a. a -> a

testAll :: QC Unit
testAll = do
  let x = consoleLog (Test1.render "String(x)" { z: "String(y.z)", y: "String(y.y)"} )
  assert true

assert :: Boolean -> QC Unit
assert = quickCheck' 1

