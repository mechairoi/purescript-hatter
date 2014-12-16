module Text.Hatter.Translator
       ( translateNode
       , requireModules )where

import Text.Hatter.Parser
import Text.Hatter.PureScript
import Data.String (joinWith)
import Data.Either

import Data.Foreign

requireModules :: [String]
requireModules = [ "VirtualDOM.VTree ()"
                 , "VirtualDOM.Typed ()"
                 , "Data.String ()" ]

translateNode :: Node -> Exp
translateNode (ElementNode tag attrs children) =
  (AppE
   (AppE
    (AppE (VarE "VirtualDOM.Typed.node") (StringLitE tag))
    (ArrayLitE $ Data.Array.map translateAttribute attrs))
   (ArrayLitE $ Data.Array.map translateNode children))

translateNode (TextNode s) = AppE (VarE "VirtualDOM.VTree.vtext") $ StringLitE s

translateNode (RawTextNode ss) = AppE (VarE "VirtualDOM.VTree.vtext") $ translateHStrings ss

translateNode (NodeExp (HExp e)) = RawE e

translateAttribute :: Attribute -> Exp
translateAttribute (Attr name value) =
  (AppE
   (AppE
    (VarE "VirtualDOM.Typed.attr")
    (translateHStrings name))
   (translateHStrings value))

translateAttribute (Toggle name) =
  (AppE
   (VarE "VirtualDOM.Typed.toggle")
   (translateHStrings name))

translateAttribute (AttributesExp (HExp e)) = RawE e

translateHStrings :: [HString] -> Exp
translateHStrings xs =
  (AppE
   (AppE
    (VarE "Data.String.joinWith")
    (StringLitE ""))
   (ArrayLitE $ Data.Array.map translateHString xs))

translateHString :: HString -> Exp
translateHString (StringLiteral s) = StringLitE s
translateHString (StringExp (HExp e)) = RawE e
