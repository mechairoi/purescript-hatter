module Text.Hatter.Translator
       ( translateNode )where

import Prelude
import Text.Hatter.Parser
import Text.Hatter.PureScript
import Data.String (joinWith)
import Data.Either

import Data.Foreign

-- import qualified Halogen.HTML.Core as C
-- import qualified Halogen.HTML.Indexed as H
-- import qualified App.Render.UnsafeCoerce as UC

translateNode :: Node -> Exp
translateNode (ElementNode tag attrs children) =
  (AppE
   (AppE
    (AppE (VarE "C.element")
      (AppE (VarE "C.tagName") (StringLitE tag)))
    (ArrayLitE $ map translateAttribute attrs))
   (ArrayLitE $ map translateNode
    children))

translateNode (TextNode s) = AppE (VarE "H.text") $ StringLitE s

translateNode (RawTextNode ss) = AppE (VarE "H.text") $ translateHStrings ss

translateNode (NodeExp (HExp e)) = AppE (VarE "UC.coerceToElement") $ RawE e

translateAttribute :: Attribute -> Exp
translateAttribute (Attr name value) =
  (AppE
    (AppE
      (AppE
        (VarE "C.prop")
        (AppE (VarE "C.propName") (translateHStrings name)))
      (AppE (VarE "C.attrName") (translateHStrings name)))
    (translateHStrings value))

translateAttribute (Toggle name) =
  (AppE
    (AppE
      (AppE
        (VarE "C.prop")
        (AppE (VarE "C.propName") (translateHStrings name)))
      (AppE (VarE "C.attrName") (translateHStrings name)))
    (VarE "true"))

translateAttribute (AttributesExp (HExp e)) =
  (AppE (VarE "UC.coerceToProperty") (RawE e))

translateHStrings :: Array HString -> Exp
translateHStrings xs =
  (AppE
   (AppE
    (VarE "Data.String.joinWith")
    (StringLitE ""))
   (ArrayLitE $ map translateHString xs))

translateHString :: HString -> Exp
translateHString (StringLiteral s) = StringLitE s
translateHString (StringExp (HExp e)) = AppE (VarE "UC.coerceToString") $ RawE e
