## Module Text.Hatter.Parser

#### `parse`

``` purescript
parse :: String -> Either ParseError Module
```

#### `Module`

``` purescript
newtype Module
  = Module (Array Declaration)
```

##### Instances
``` purescript
Eq Module
```

#### `Declaration`

``` purescript
newtype Declaration
  = Declaration { rawCode :: String, body :: Node }
```

##### Instances
``` purescript
Eq Declaration
```

#### `Node`

``` purescript
data Node
  = ElementNode TagName (Array Attribute) (Array Node)
  | TextNode String
  | RawTextNode (Array HString)
  | NodeExp HExp
```

##### Instances
``` purescript
Eq Node
```

#### `TagName`

``` purescript
type TagName = String
```

#### `Attribute`

``` purescript
data Attribute
  = Attr AttributeName AttributeValue
  | Toggle AttributeName
  | AttributesExp HExp
```

##### Instances
``` purescript
Eq Attribute
```

#### `AttributeName`

``` purescript
type AttributeName = Array HString
```

#### `AttributeValue`

``` purescript
type AttributeValue = Array HString
```

#### `HExp`

``` purescript
newtype HExp
  = HExp String
```

##### Instances
``` purescript
Eq HExp
```

#### `HString`

``` purescript
data HString
  = StringLiteral String
  | StringExp HExp
```

##### Instances
``` purescript
Eq HString
```


