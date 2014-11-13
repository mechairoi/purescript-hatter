module Text.Hatter.PureScript
       ( Exp(..)
       , toCode
       , Name()
       ) where

import Data.String (joinWith)
import Data.Array


-- class Pat a
-- class Dec a
-- class Type a

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

toCode (RawE rawCode) = joinWith " " [ "(", rawCode, ")"]

escapeString :: String -> String
escapeString str = str -- XXX implement

-- newtype ConP = ConP Name [Pat]
-- instance conPat (Pat ConP)
-- wildP = PatL "_"

-- newtype LamE = LamdaL [Pat] Exp
-- instance lamExp (Exp LamE)

-- newtype FunD = FunD
-- newtype DataD = DataD
