module Test.Text.Hatter.Parser where

import Prelude
import Test.QuickCheck
import Text.Hatter.Parser
import Control.Monad.Eff
import Debug.Trace
import Data.Either
import Text.Parsing.Parser

testAll :: forall eff. QC eff Unit
testAll = do
  testBody "  <div></div>" $ ElementNode "div" [] []

  testBody "  <div><hr></div>" $ ElementNode "div" [] [ ElementNode "hr" [] [] ]

  testBody "  foo" $ TextNode "  foo"

  testBody "  <div>foo</div>" $ ElementNode "div" [] [ TextNode "foo" ]

  testBody "  <textarea><div>foo</div></textarea>" $ ElementNode "textarea" []
    [ RawTextNode [ StringLiteral "<div>foo</div>"] ]

  testBody "  <textarea><div>foo</<% x %>div></textarea>" $ ElementNode "textarea" []
    [ RawTextNode [ StringLiteral "<div>foo</"
                  , StringExp $ HExp " x "
                  , StringLiteral "div>" ] ]

  testBody "  <div>foo<% n %></div>" $ ElementNode "div" []
    [ TextNode "foo"
    , NodeExp $ HExp " n " ]

  testBody """  <div attr="value"></div>""" $
    ElementNode "div" [ Attr "attr" [ StringLiteral "value" ] ] []

  testBody """  <div attr=value attr2='value2' <% n %>></div>""" $
    ElementNode "div" [ Attr "attr" [ StringLiteral "value"  ]
                      , Attr "attr2" [ StringLiteral "value2" ]
                      , AttributesExp $ HExp " n "
                      ] []

  testBody "  <div <% n %>></div>" $ ElementNode "div" [ AttributesExp $ HExp " n "] []

  testBody "  <div id=<% n %>></div>" $ ElementNode "div"
    [ Attr "id" [ StringExp $ HExp " n " ] ] []

  testBody "  <div id='&amp;&gt;&lt;'>&amp;&gt;&lt;</div>" $ ElementNode "div"
    [ Attr "id"[ StringLiteral "&><" ] ]
    [ TextNode "&><" ]

testBody :: forall eff. String -> Node -> QC (|eff) Unit
testBody input expected = do
  traceAnyM $ "parse node: " <> input
  -- traceAnyM $ "result: "
  -- traceAnyM $ (parse $ rawCode <> input)
  assert $ eqRight (parse $ rawCode <> input) $
    Module [ Declaration { rawCode: rawCode
                         , body: expected } ]
  where rawCode = "module UnitTest.Hatter.Parser.Sample where\nrender :: VTree\nrender =\n"

eqRight :: forall a b. (Eq b) => Either a b -> b -> Boolean
eqRight (Right x) y = x == y
eqRight (Left _) _ = false

assert :: forall eff. Boolean -> QC eff Unit
assert = quickCheck' 1
