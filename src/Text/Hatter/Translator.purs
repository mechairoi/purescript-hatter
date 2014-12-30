module Text.Hatter.Translator
       ( translateNode )where

import Text.Hatter.Parser
import Text.Hatter.PureScript
import Data.String (joinWith)
import Data.Either
import Data.Array

translateNode :: Node -> Exp
translateNode (ElementNode tag attrs children) =
  (AppE
   (AppE
    (AppE
     (AppE
      (AppE (VarE "VirtualDOM.VTree.Typed.vnode") (StringLitE tag))
      $ translateAttributes attrs)
     $ translateNodes children)
    (VarE "Data.Maybe.Nothing"))
   (VarE "Data.Maybe.Nothing"))

translateNode (TextNode s) = AppE (VarE "VirtualDOM.VTree.Typed.vtext") $ StringLitE s

translateNode (RawTextNode ss) = AppE (VarE "VirtualDOM.VTree.Typed.vtext") $ translateHStrings ss

translateNode (NodeExp (HExp e)) = RawE e

translateNodes :: [Node] -> Exp
translateNodes children =
  AppE (VarE "Data.Array.concat")
    $ ArrayLitE
    $ AppE (VarE "Text.Hatter.Runtime.coerceToVTrees")
    <$> translateNode
    <$> children

translateAttributes :: [Attribute] -> Exp
translateAttributes children =
  AppE (VarE "Data.Array.concat")
    $ ArrayLitE
    $ AppE (VarE "Text.Hatter.Runtime.coerceToAttributes")
    <$> translateAttribute
    <$> children

translateAttribute :: Attribute -> Exp
translateAttribute (Attr name value) =
  (AppE
   (AppE
    (VarE "VirtualDOM.VTree.Typed.attr")
    (translateHStrings name))
   (translateHStrings value))

translateAttribute (Toggle name) =
  (AppE
   (AppE
    (VarE "VirtualDOM.VTree.Typed.toggle")
    (translateHStrings name))
   (VarE "true"))

translateAttribute (AttributesExp (HExp e)) = RawE e

translateHStrings :: [HString] -> Exp
translateHStrings xs =
  (AppE
   (AppE
    (VarE "Data.String.joinWith")
    (StringLitE ""))
   (ArrayLitE $ translateHString <$> xs))

translateHString :: HString -> Exp
translateHString (StringLiteral s) = StringLitE s
translateHString (StringExp (HExp e)) =
  AppE (VarE "Text.Hatter.Runtime.coerceToString") $ RawE e
