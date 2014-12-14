module Index where
import Text.Hatter (hatter)
import Control.Monad.Eff

main = exportModule hatter

foreign import exportModule "function exportModule(x) { return function() { module.exports = x; };  }" :: forall a eff. a -> Eff eff Unit
