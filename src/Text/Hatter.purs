module Text.Hatter
       ( hatter ) where

import Data.Either
import Data.String (joinWith)
import Text.Parsing.Parser (ParseError())
import Text.Hatter.PureScript
import Text.Hatter.Translator (translateNode)
import Text.Hatter.Parser

hatter :: [String] -> String -> Either ParseError String
hatter imports input = do
  (Module codes) <- parse input
  return $ joinWith "" $
    (translateCode <$> codes) ++
      ["\n"] ++
      ((\i -> "import " ++ i ++ "\n") <$> imports)
  where
    translateCode (RawCode c) = c
    translateCode (Template node) = (toCode $ translateNode node)
