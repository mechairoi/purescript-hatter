## Module Text.Hatter.PureScript

#### `Name`

``` purescript
type Name = String
```

#### `Exp`

``` purescript
data Exp
  = VarE Name
  | AppE Exp Exp
  | StringLitE String
  | ArrayLitE (Array Exp)
  | RawE String
  | SigE Exp Name
```

#### `toCode`

``` purescript
toCode :: Exp -> String
```


