module Text.Hatter.Translator
       ( translateNode )where

import Prelude
import Text.Hatter.Parser
import Text.Hatter.PureScript
import Data.String (joinWith)
import Data.Either
import Data.StrMap as M
import Data.Tuple
import Data.Foreign

translateNode :: Node -> Exp
translateNode (ElementNode tag attrs children) =
  (AppE
   (AppE
    (AppE (VarE "VirtualDOM.VTree.vnode") (StringLitE tag))
    (AppE (VarE "Text.Hatter.Runtime.toAttributes")
     (ArrayLitE $ map translateAttribute attrs)))
   (ArrayLitE $ map translateNode
    children))

translateNode (TextNode s) = AppE (VarE "VirtualDOM.VTree.vtext") $ StringLitE s

translateNode (RawTextNode ss) = AppE (VarE "VirtualDOM.VTree.vtext") $ translateHStrings ss

translateNode (NodeExp (HExp e)) = AppE (VarE "Text.Hatter.Runtime.coerce") $ RawE e

translateAttribute :: Attribute -> Exp
translateAttribute (Attr name value) =
  RecordLitE (M.singleton name (translateHStrings value))

translateAttribute (Toggle name) =
  RecordLitE (M.singleton name (VarE "true"))

translateAttribute (AttributesExp (HExp e)) =
  AppE (VarE "Text.Hatter.Runtime.coerce") (RawE e)

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
