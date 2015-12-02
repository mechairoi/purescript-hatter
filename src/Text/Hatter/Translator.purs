module Text.Hatter.Translator
       ( translateNode )where

import Prelude
import Text.Hatter.Parser
import Text.Hatter.PureScript
import Data.String (joinWith)
import Data.Either

import Data.Foreign

translateNode :: Node -> Exp
translateNode (ElementNode tag attrs children) =
  (AppE
   (AppE
    (AppE
     (AppE
      (AppE (VarE "VirtualDOM.VTree.Typed.vnode") (StringLitE tag))
      (ArrayLitE $ map translateAttribute attrs))
     (ArrayLitE $ map translateNode
      children))
    (VarE "Data.Maybe.Nothing"))
   (VarE "Data.Maybe.Nothing"))

translateNode (TextNode s) = AppE (VarE "VirtualDOM.VTree.Typed.vtext") $ StringLitE s

translateNode (RawTextNode ss) = AppE (VarE "VirtualDOM.VTree.Typed.vtext") $ translateHStrings ss

translateNode (NodeExp (HExp e)) = AppE (VarE "Text.Hatter.Runtime.coerce") $ RawE e

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

translateAttribute (AttributesExp (HExp e)) =
  (AppE (VarE "Text.Hatter.Runtime.coerce") (RawE e))

translateHStrings :: Array HString -> Exp
translateHStrings xs =
  (AppE
   (AppE
    (VarE "Data.String.joinWith")
    (StringLitE ""))
   (ArrayLitE $ map translateHString xs))

translateHString :: HString -> Exp
translateHString (StringLiteral s) = StringLitE s
translateHString (StringExp (HExp e)) = AppE (VarE "Text.Hatter.Runtime.coerce") $ RawE e
