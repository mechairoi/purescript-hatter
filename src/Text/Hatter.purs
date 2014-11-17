module Text.Hatter
       ( hatter ) where

import Data.Either
import Data.String (joinWith)
import Text.Parsing.Parser (ParseError())
import Text.Hatter.PureScript
import Text.Hatter.Translator (translateNode, requireModules)
import Text.Hatter.Parser

hatter :: String -> [String] -> String -> Either ParseError String
hatter moduleName imports input = do
  (Document doc) <- parse input
  return $ joinWith "" ( [ "module "
                         , moduleName
                         , " where \n" ] ++
                         (Data.Array.map (\i -> "import " ++ i ++ "\n") is) ++
                         [ doc.typeAnnotation
                         , "\n"
                         , doc.args
                         , toCode $ translateNode doc.body ] )
  where is = imports ++ requireModules
