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
    (AppE (VarE "Text.Hatter.Runtime.vnode") (StringLitE tag))
    (ArrayLitE $ translateAttribute <$> attrs))
   (AppE (VarE "Text.Hatter.Runtime.fold")
    (ArrayLitE $ translateNodes <$> children))
   )

translateNode (TextNode s) = AppE (VarE "Text.Hatter.Runtime.vtext") $ StringLitE s

translateNode (RawTextNode ss) = AppE (VarE "Text.Hatter.Runtime.vtext") $ translateHStrings ss

translateNode (NodeExp (HExp e)) = AppE (VarE "Text.Hatter.Runtime.toVTree") $ RawE e

translateNodes :: Node -> Exp
translateNodes (NodeExp (HExp e)) = AppE (VarE "Text.Hatter.Runtime.toVTrees") $ RawE e
translateNodes n = ArrayLitE [ translateNode n ]

translateAttribute :: Attribute -> Exp
translateAttribute (Attr name value) =
  AppE (VarE "Text.Hatter.Runtime.attr") (RecordLitE (M.singleton (translateAttributeName name) (translateHStrings value)))

translateAttribute (Toggle name) =
  AppE (VarE "Text.Hatter.Runtime.attr") (RecordLitE (M.singleton (translateAttributeName name) (VarE "true")))

translateAttribute (AttributesExp (HExp e)) =
  AppE (VarE "Text.Hatter.Runtime.attr") (RawE e)

translateAttributeName :: String -> String
translateAttributeName "class" = "className"
translateAttributeName "for" = "htmlFor"
translateAttributeName name = name

translateHStrings :: Array HString -> Exp
translateHStrings xs =
  (AppE
   (AppE
    (VarE "Text.Hatter.Runtime.joinWith")
    (StringLitE ""))
   (ArrayLitE $ translateHString <$> xs))

translateHString :: HString -> Exp
translateHString (StringLiteral s) = StringLitE s
translateHString (StringExp (HExp e)) = RawE e
