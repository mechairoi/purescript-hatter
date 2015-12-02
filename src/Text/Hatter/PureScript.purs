module Text.Hatter.PureScript
       ( Exp(..)
       , toCode
       , Name()
       ) where

import Prelude
import Data.String (joinWith)
import Data.Array
import Data.String.Regex

type Name = String

data Exp = VarE Name
         | AppE Exp Exp
         | StringLitE String | ArrayLitE (Array Exp)
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
   "[" ++ joinWith ", " (map toCode exps) ++ "]"

toCode (RawE rawCode) = joinWith " " [ "("
                                     , rawCode
                                     , "\n  )" ]

toCode (SigE exp signature) = joinWith " " [ "("
                                           , toCode exp
                                           , "::"
                                           , signature
                                           , ")" ]

escapeString :: String -> String
escapeString str = replace (regex "\a"   $ parseFlags "gm") "\\a"  $
                   replace (regex "\b"   $ parseFlags "gm") "\\b"  $
                   replace (regex "\f"   $ parseFlags "gm") "\\f"  $
                   replace (regex "\n"   $ parseFlags "gm") "\\n"  $
                   replace (regex "\t"   $ parseFlags "gm") "\\t"  $
                   replace (regex "\r"   $ parseFlags "gm") "\\r"  $
                   replace (regex "\v"   $ parseFlags "gm") "\\v"  $
                   replace (regex "\""   $ parseFlags "gm") "\\\"" $
                   replace (regex "\'"   $ parseFlags "gm") "\\\'" $
                   replace (regex "\\\\" $ parseFlags "gm") "\\\\" str

