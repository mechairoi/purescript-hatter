module Text.Hatter.PureScript
       ( Exp(..)
       , toCode
       , Name()
       ) where

import Data.String (joinWith, replace)
import Data.Array

type Name = String

data Exp = VarE Name
         | AppE Exp Exp
         | StringLitE String | ArrayLitE [Exp]
         | RawE String

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
   "[" ++ joinWith ", " (Data.Array.map toCode exps) ++ "]"

toCode (RawE rawCode) = joinWith " " [ "(", rawCode, "\n  )"]

escapeString :: String -> String
escapeString str = replace "\a" "\\a"  $
                   replace "\b" "\\b"  $
                   replace "\f" "\\f"  $
                   replace "\n" "\\n"  $
                   replace "\t" "\\t"  $
                   replace "\r" "\\r"  $
                   replace "\v" "\\v"  $
                   replace "\"" "\\\"" $
                   replace "\'" "\\\'" $
                   replace "\\" "\\\\" str
