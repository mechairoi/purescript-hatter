module Text.Hatter.PureScript
       ( Exp(..)
       , toCode
       , Name()
       ) where

import Prelude
import Data.String (joinWith)
import Data.Array
import Data.String.Regex
import Data.StrMap as M
import Data.Either (fromRight)
import Data.Tuple
import Partial.Unsafe (unsafePartial)

type Name = String

data Exp = VarE Name
         | AppE Exp Exp
         | StringLitE String
         | ArrayLitE (Array Exp)
         | RecordLitE (M.StrMap Exp)
         | RawE String
         | SigE Exp Name

toCode :: Exp -> String
toCode (VarE name) = name
toCode (AppE f arg) = joinWith " " [ "("
                                   , toCode f
                                   , toCode arg
                                   , ")"
                                   ]
toCode (StringLitE name) = joinWith "" [ "\""
                                       , escapeString name
                                       , "\""
                                       ]
toCode (ArrayLitE exps) =
   "[" <> joinWith ", " (map toCode exps) <> "]"

toCode (RecordLitE smap) =
  "{" <> joinWith ", " (pairToCode <$> M.toUnfoldable smap) <> "}"
  where pairToCode (Tuple key valueExp) = toCode (StringLitE key) <> ":" <> toCode valueExp

toCode (RawE rawCode) = joinWith " " [ "("
                                     , rawCode
                                     , "\n  )" ]

toCode (SigE exp signature) = joinWith " " [ "("
                                           , toCode exp
                                           , "::"
                                           , signature
                                           , ")" ]

escapeString :: String -> String
escapeString str = replace (unsafePartial fromRight $ regex "\a"   $ parseFlags "gm") "\\a"  $
                   replace (unsafePartial fromRight $ regex "\b"   $ parseFlags "gm") "\\b"  $
                   replace (unsafePartial fromRight $ regex "\f"   $ parseFlags "gm") "\\f"  $
                   replace (unsafePartial fromRight $ regex "\n"   $ parseFlags "gm") "\\n"  $
                   replace (unsafePartial fromRight $ regex "\t"   $ parseFlags "gm") "\\t"  $
                   replace (unsafePartial fromRight $ regex "\r"   $ parseFlags "gm") "\\r"  $
                   replace (unsafePartial fromRight $ regex "\v"   $ parseFlags "gm") "\\v"  $
                   replace (unsafePartial fromRight $ regex "\""   $ parseFlags "gm") "\\\"" $
                   replace (unsafePartial fromRight $ regex "\'"   $ parseFlags "gm") "\\\'" $
                   replace (unsafePartial fromRight $ regex "\\\\" $ parseFlags "gm") "\\\\" str

