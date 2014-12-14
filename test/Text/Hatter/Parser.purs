module Test.Text.Hatter.Parser where

import Test.QuickCheck
import Text.Hatter.Parser
import Control.Monad.Eff
import Debug.Trace
import Data.Either
import Text.Parsing.Parser

testAll :: QC Unit
testAll = do
  testBody "<div></div>" $ ElementNode "div" [] []

  testBody "<div><hr></div>" $ ElementNode "div" [] [ ElementNode "hr" [] [] ]

  testBody "foo" $ TextNode "foo"

  testBody "<div>foo</div>" $ ElementNode "div" [] [ TextNode "foo" ]

  testBody "<textarea><div>foo</div></textarea>" $ ElementNode "textarea" []
    [ RawTextNode [ StringLiteral "<div>foo</div>"] ]

  testBody "<textarea><div>foo</<% x %>div></textarea>" $ ElementNode "textarea" []
    [ RawTextNode [ StringLiteral "<div>foo</"
                  , StringExp $ HExp " x "
                  , StringLiteral "div>" ] ]

  testBody "<div>foo<% n %></div>" $ ElementNode "div" []
    [ TextNode "foo"
    , NodeExp $ HExp " n " ]

  testBody """<div attr="value"></div>""" $
    ElementNode "div" [ Attr [ StringLiteral "attr" ] [ StringLiteral "value" ] ] []

  testBody """<div attr=value attr2='value2' <% n %>></div>""" $
    ElementNode "div" [ Attr [ StringLiteral "attr"  ] [ StringLiteral "value"  ]
                      , Attr [ StringLiteral "attr2" ] [ StringLiteral "value2" ]
                      , AttributesExp $ HExp " n "
                      ] []

  testBody "<div <% n %>></div>" $ ElementNode "div" [ AttributesExp $ HExp " n "] []

  testBody "<div id=<% n %>></div>" $ ElementNode "div"
    [ Attr [ StringLiteral "id" ] [ StringExp $ HExp " n " ] ] []

  testBody "<div id='&amp;&gt;&lt;'>&amp;&gt;&lt;</div>" $ ElementNode "div"
    [ Attr [ StringLiteral "id" ] [ StringLiteral "&><" ] ]
    [ TextNode "&><" ]

testBody :: String -> Node -> QC Unit
testBody input expected = do
  trace $ "parse node: " ++ input
  assert $ eqRight (parse $ "render :: VTree\nrender =\n" ++ input) $
    Document { typeAnnotation: "render :: VTree"
             , args: "render ="
             , body: expected
             }

eqRight :: forall a b. (Eq b) => Either a b -> b -> Boolean
eqRight (Right x) y = x == y
eqRight (Left _) _ = false

assert :: Boolean -> QC Unit
assert = quickCheck' 1
