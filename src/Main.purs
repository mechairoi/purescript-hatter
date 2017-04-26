module Main where

import Text.Hatter (hatter)
import Control.Monad.Eff (kind Effect, Eff)
import Data.Either (Either, isRight)
import Prelude (Unit)
import Text.Parsing.Parser (ParseError())

foreign import data MAIN :: Effect

main :: forall e. Eff (main :: MAIN | e) Unit
main = mainImpl hatter isRight

foreign import mainImpl :: forall eff l r. (Array String -> String -> Either ParseError String) -> (Either l r -> Boolean) -> Eff (main :: MAIN | eff) Unit
