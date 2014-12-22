module Text.Hatter
       ( hatter ) where

import Data.Either
import Data.String (joinWith)
import Data.Array (map)
import Text.Parsing.Parser (ParseError())
import Text.Hatter.PureScript
import Text.Hatter.Translator (translateNode)
import Text.Hatter.Parser

hatter :: [String] -> String -> Either ParseError String
hatter imports input = do
  (Module decs) <- parse input
  return $ joinWith "" $
    map translateDeclaration decs ++ map (\i -> "import " ++ i ++ "\n") imports
  where translateDeclaration (Declaration d) =
          d.rawCode ++ "  " ++ (toCode $ translateNode d.body) ++ "\n\n"
