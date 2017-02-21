module Main where

import Text.Hatter (hatter)
import Control.Monad.Eff
import Data.Either
import Prelude
import Text.Parsing.Parser (ParseError())

foreign import data MAIN :: !

main = mainImpl hatter isRight

foreign import mainImpl :: forall eff l r. (Array String -> String -> Either ParseError String) -> (Either l r -> Boolean) -> Eff (main :: MAIN | eff) Unit
