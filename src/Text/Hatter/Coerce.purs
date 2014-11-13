module Text.Hatter.Coerce
       ( Coerce
       , coerce ) where

import VirtualDOM
import VirtualDOM.VTree
import VirtualDOM.Typed

class Coerce a b where
  coerce :: a -> b

instance idCoerce :: Coerce a a where
  coerce = id

instance stringNodeCoerce :: Coerce String VTree where
  coerce s = vtext s

instance attributesCoerce :: (Attribute a) => Coerce a [a] where
  coerce a = [a]

instance nodesCoerce :: Coerce VTree [VTree] where
  coerce a = [a]
