# Module Documentation

## Module Text.Hatter.Coerce

### Type Classes

    class Coerce a b where
      coerce :: a -> b


### Type Class Instances

    instance attributesCoerce :: (Attribute a) => Coerce a [a]

    instance idCoerce :: Coerce a a

    instance nodesCoerce :: Coerce VTree [VTree]

    instance stringNodeCoerce :: Coerce String VTree


## Module Text.Hatter.Parser

### Types

    data Attribute where
      Attr :: AttributeName -> AttributeValue -> Attribute
      Toggle :: AttributeName -> Attribute
      AttributesExp :: HExp -> Attribute

    type AttributeName = [HString]

    type AttributeValue = [HString]

    newtype Document where
      Document :: { body :: Node, args :: String, typeAnnotation :: String } -> Document

    newtype HExp where
      HExp :: String -> HExp

    data HString where
      StringLiteral :: String -> HString
      StringExp :: HExp -> HString

    data Node where
      ElementNode :: TagName -> [Attribute] -> [Node] -> Node
      TextNode :: String -> Node
      RawTextNode :: [HString] -> Node
      NodeExp :: HExp -> Node

    type TagName = String


### Type Class Instances

    instance eqAttribute :: Eq Attribute

    instance eqDocument :: Eq Document

    instance eqHExp :: Eq HExp

    instance eqHString :: Eq HString

    instance eqNode :: Eq Node


### Values

    parse :: String -> Either ParseError Document


## Module Text.Hatter.PureScript

### Types

    data Exp where
      VarE :: Name -> Exp
      AppE :: Exp -> Exp -> Exp
      StringLitE :: String -> Exp
      ArrayLitE :: [Exp] -> Exp
      RawE :: String -> Exp

    type Name = String


### Values

    toCode :: Exp -> String


## Module Text.Hatter.Translator

### Values

    translateAttribute :: Attribute -> Exp

    translateHString :: HString -> Exp

    translateHStrings :: [HString] -> Exp

    translateNode :: Node -> Exp



