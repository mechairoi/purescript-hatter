module Text.Hatter
       ( hatter ) where

import Data.Either
import Data.String (joinWith)
import Text.Parsing.Parser (ParseError())
import Text.Hatter.PureScript
import Text.Hatter.Translator
import Text.Hatter.Parser

hatter :: String -> String -> Either ParseError String
hatter input moduleName = do
  (Document doc) <- parse input
  return $ joinWith "" [ "module "
                       , moduleName
                       , " where \n"
                       , doc.typeAnnotation
                       , "\n"
                       , doc.args
                       , toCode $ translateNode doc.body
                       ]
