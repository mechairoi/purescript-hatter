module Test.Integration where

import Test.QuickCheck
import Control.Monad.Eff
import Debug.Trace
import Prelude
import Text.Hatter.Runtime (vnode, toVTree, attr)
import VirtualDOM (PatchObject, diff) as V
import VirtualDOM.VTree (vnode, vtext) as V
import Test.Fixture.Test1 as Test1

testAll :: forall eff. QC eff Unit
testAll = do
  traceAnyM $ "Test.Integration: Test1.render"
  traceAnyM $ (Test1.render "String(x)" { z: "String(y.z)", y: "String(y.y)"} )
  assert $ isEmptyPatchObject $ V.diff
    (Test1.render "String(x)" { z: "String(y.z)", y: "String(y.y)"} ) $
    V.vnode "div" {}
      [ V.vnode "span" {}
        [ V.vtext "String(x)"
        , V.vtext " foo"
        ]
      , V.vtext "String(y.z)"
      , V.vtext "\n    &><\n    "
      , V.vnode "textarea" {} [ V.vtext "<script></script>" ]
      , V.vnode "hr" {} []
      , V.vnode "input" { type: "hidden"
                        , name: "bar"
                        , value: "baString(y.y)z" } []
      ]

assert :: forall eff. Boolean -> QC eff Unit
assert = quickCheck' 1

foreign import isEmptyPatchObject :: V.PatchObject -> Boolean
