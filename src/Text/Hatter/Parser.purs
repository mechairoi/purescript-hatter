module Text.Hatter.Parser
       ( parse
       , Node(..)
       , Attribute(..)
       , HString(..)
       , HExp(..)
       , Module(..)
       , Declaration(..)
       , TagName(..)
       , AttributeValue(..)
       , AttributeName(..)
       ) where

import Data.String
import Data.Either
import Data.Tuple
import Control.Alternative(many)

import Control.Alt
import Control.Monad.Error(strMsg)

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String
import Prelude

parse :: String -> Either ParseError Module
parse input = runParser input pModule

newtype Module = Module [Declaration]

newtype Declaration = Declaration { rawCode :: String
                                  , body :: Node }

instance eqModule :: Eq Module where
  (==) (Module a) (Module a') = a == a'
  (/=) a a' = not $ a == a'

instance eqDeclaration :: Eq Declaration where
  (==) (Declaration a) (Declaration a') = a.rawCode == a'.rawCode && a.body == a'.body
  (/=) a a' = not $ a == a'

pModule :: forall m. (Monad m) => ParserT String m Module
pModule = do
  skipEmptyLines
  decs <- many pDeclaration
  eof
  return $ Module decs

pIndent  :: forall m. (Monad m) => ParserT String m Unit
pIndent = unify $ oneOf [" ", "\t"]

skipEmptyLines :: forall m. (Monad m) => ParserT String m Unit
skipEmptyLines = do
  skipMany $ try emptyLine
  where emptyLine = do
          many pIndent
          oneOf ["\n", "\r"]
          return unit

pDeclaration :: forall m. (Monad m) => ParserT String m Declaration
pDeclaration = do
  rawCodes <- line `many1Till` lookAhead pIndent
  body <- pNode
  skipEmptyLines
  return $ Declaration { rawCode: joinWith "" rawCodes, body: body }

line :: forall m. (Monad m) => ParserT String m String
line = do
  l <- stringTill $ string "\n"
  return $ l ++ "\n"

data Node = ElementNode TagName [Attribute] [Node]
          | TextNode String
          | RawTextNode [HString]
          | NodeExp HExp

instance eqNode :: Eq Node where
  (==) (ElementNode a b c) (ElementNode a' b' c') = a == a' && b == b' && c == c'
  (==) (TextNode a) (TextNode a') = a == a'
  (==) (RawTextNode a) (RawTextNode a') = a == a'
  (==) (NodeExp a) (NodeExp a') = a == a'
  (==) _ _ = false
  (/=) a a' = not $ a == a'

pNode :: forall m. (Monad m) => ParserT String m Node
pNode = (try pNodeExp <|> try pElementNode <|> pTextNode)

pNodeExp :: forall m. (Functor m, Monad m) => ParserT String m Node -- NodeExp
pNodeExp = do
  skipSpaces
  NodeExp <$> pHExp

pTextNode :: forall m. (Functor m, Monad m) => ParserT String m Node -- TextNod
pTextNode = do
  s <- stringTill $ lookAhead $ do
    (unify $ string "<") <|> eof
  return $ TextNode $ unescapeHtml s

pElementNode :: forall m. (Monad m) => ParserT String m Node -- ElementNode
pElementNode = (try pVoidElement) <|> (try pRawElement) <|> (try pEscapableRawElement) <|> pNormalElement

pVoidElement :: forall m. (Monad m) => ParserT String m Node -- ElementNode
pVoidElement = do
  skipSpaces
  Tuple tag attrs <- pStartTagVoid
  return $ ElementNode tag attrs []
  where pStartTagVoid = pStartTag (pTagNameOneOf voidElementTags end) true
        end = someWhiteSpaces <|> (choice $ Data.Array.map string [">", "/>"])

voidElementTags :: [TagName]
voidElementTags = ["area", "base", "br", "col", "embed", "hr", "img", "input", "keygen", "link", "menuitem", "meta", "param", "source", "track", "wbr"]

pRawElement :: forall m. (Monad m) => ParserT String m Node -- ElementNode
pRawElement = do
  skipSpaces
  Tuple tag attrs <- pStartTagRaw
  children <- pRawTextNode (pEndTag tag) `manyTill` (lookAhead $ pEndTag tag)
  pEndTag tag
  return $ ElementNode tag attrs children
  where pStartTagRaw = pStartTag (pTagNameOneOf rawElementTags end) false
        end = someWhiteSpaces <|> string ">"

rawElementTags :: [TagName]
rawElementTags = ["script", "style"]

pEscapableRawElement :: forall m. (Monad m) => ParserT String m Node -- ElementNode
pEscapableRawElement = do
  skipSpaces
  Tuple tag attrs <- pStartTagEscapableRaw
  children <- pRawTextNode (pEndTag tag) `manyTill` (lookAhead $ pEndTag tag)
  pEndTag tag
  return $ ElementNode tag attrs children
  where pStartTagEscapableRaw = pStartTag (pTagNameOneOf escapableRawElementTags end) false
        end = someWhiteSpaces <|> string ">"

escapableRawElementTags :: [TagName]
escapableRawElementTags = ["textarea", "title"]

pRawTextNode :: forall a m. (Functor m, Monad m) => ParserT String m a -> ParserT String m Node -- TextNod
pRawTextNode end = do
  RawTextNode <$> pHStrings end

pNormalElement :: forall m. (Monad m) => ParserT String m Node -- ElementNode
pNormalElement = do
  skipSpaces
  Tuple tag attrs <- pStartTag (pTagName end) false
  children <- pNode `manyTill` (lookAhead $ pEndTag tag)
  pEndTag tag
  return $ ElementNode tag attrs children
  where end = someWhiteSpaces <|> string ">"

pStartTag :: forall m. (Monad m) => ParserT String m TagName -> Boolean -> ParserT String m (Tuple TagName [Attribute])
pStartTag pTagName allowSlash = do
  skipSpaces
  string "<"
  tag <- pTagName
  attributes <- pAttributes end
  skipSpaces
  end
  return $ Tuple tag attributes
  where
    end = do
      skipSpaces
      if allowSlash
        then choice $ Data.Array.map string [">", "/>"]
        else string ">"

pEndTag :: forall m. (Monad m) => TagName -> ParserT String m Unit
pEndTag tag = do
  skipSpaces
  string $ joinWith "" [ "</", tag, ">" ]
  return unit

type TagName = String

pTagName :: forall a m. (Monad m) => ParserT String m a -> ParserT String m TagName
pTagName end = do
  skipSpaces
  stringTill $ lookAhead end

pTagNameOneOf :: forall a m. (Monad m) => [TagName] -> ParserT String m a -> ParserT String m TagName
pTagNameOneOf tags end = do
  skipSpaces
  tag <- choice $ Data.Array.map string tags
  lookAhead end
  return tag

pAttributes :: forall a m. (Monad m) => ParserT String m a -> ParserT String m [Attribute]
pAttributes end = do
  skipSpaces
  pAttribute end `manyTill` lookAhead end

pAttributesEnd  :: forall m. (Monad m) => ParserT String m Unit
pAttributesEnd = lookAhead do
  skipSpaces
  string ">" <|> string "/>"
  return unit

-- embedded purescript expiression
data Attribute = Attr AttributeName AttributeValue
               | Toggle AttributeName
               -- | Style [CSSProperty] --TODO: implement
               -- | Class [ClassName] --TODO: implement
               | AttributesExp HExp

instance eqAttribute :: Eq Attribute where
  (==) (Attr a b) (Attr a' b') = a == a' && b == b'
  (==) (Toggle a) (Toggle a') = a == a'
  (==) (AttributesExp a) (AttributesExp a') = a == a'
  (==) _ _ = false
  (/=) a a' = not $ a == a'

pAttribute :: forall a m. (Monad m) => ParserT String m a -> ParserT String m Attribute
pAttribute attrsEnd = do
  skipSpaces
  try pAttributesExp <|> try (pAttr end) <|> pToggle end
  where
    end = (unify $ string " ") <|> (unify attrsEnd)

pAttributesExp :: forall m. (Functor m, Monad m) => ParserT String m Attribute
pAttributesExp = AttributesExp <$> pHExp

pAttr :: forall a m. (Monad m) => ParserT String m a -> ParserT String m Attribute
pAttr end = do
  name <- pHStrings $ string "="
  string "="
  value <- pAttributeValue end
  return $ Attr name value

pToggle :: forall a m. (Monad m) => ParserT String m a -> ParserT String m Attribute -- Toggle
pToggle end = do
  name <- pHStrings end
  return $ Toggle name

type AttributeName = [HString]

pAttributeName :: forall a m. (Monad m) => ParserT String m a -> ParserT String m AttributeName
pAttributeName end = pHStrings end

type AttributeValue = [HString]

pAttributeValue :: forall a m. (Monad m) => ParserT String m a -> ParserT String m AttributeValue
pAttributeValue end = pDoubleQuotedHStrings <|> pSingleQuotedHStrings  <|> pHStrings end

pSingleQuotedHStrings :: forall m. (Monad m) => ParserT String m [HString]
pSingleQuotedHStrings = between sq sq $ pHStrings sq
  where sq = string "'"

pDoubleQuotedHStrings :: forall m. (Monad m) => ParserT String m [HString]
pDoubleQuotedHStrings = between dq dq $ pHStrings dq
  where dq = string "\""

newtype HExp = HExp String
instance eqHExp :: Eq HExp where
  (==) (HExp a) (HExp a') = a == a'
  (/=) a a' = not $ a == a'

pHExp :: forall m. (Monad m) => ParserT String m HExp
pHExp = do
  string "<%"
  body <- stringTill $ string "%>"
  return $ HExp body

data HString = StringLiteral String | StringExp HExp

instance eqHString :: Eq HString where
  (==) (StringLiteral a) (StringLiteral a') = a == a'
  (==) (StringExp a) (StringExp a') = a == a'
  (==) _ _ = false
  (/=) a a' = not $ a == a'

unify :: forall a m. (Functor m, Monad m) => ParserT String m a -> ParserT String m Unit
unify parser = (\_ -> unit) <$> parser

pHStrings :: forall a m. (Monad m) => ParserT String m a -> ParserT String m [HString]
pHStrings end = (pHString $ lookAhead ((unify $ string "<%") <|> (unify end))) `manyTill` (lookAhead end)

pHString :: forall a m. (Functor m, Monad m) => ParserT String m a -> ParserT String m HString
pHString end = (StringExp <$> pHExp) <|> (StringLiteral <<< unescapeHtml <$> stringTill (lookAhead end))

stringTill :: forall a m. (Functor m, Monad m) => ParserT String m a -> ParserT String m String
stringTill end = joinWith "" <$> (char `manyTill` end)

someWhiteSpaces :: forall m. (Functor m, Monad m) => ParserT String m String
someWhiteSpaces = do
  let space = oneOf ["\n", "\r", " ", "\t"]
  x <- space
  xs <- whiteSpace
  return $ x ++ xs

stringi :: forall a m. (Monad m) => String -> ParserT String m String
stringi s = ParserT $ \s' ->
  return $ case toLower $ take (length s) s' of
    s'' | s'' == toLower s -> { consumed: true, input: drop (length s) s', result: Right s }
    _ -> { consumed: false, input: s', result: Left (strMsg ("Expected " ++ show s)) }

unescapeHtml :: String -> String
unescapeHtml html = replace "&amp;" "&" $ replace "&lt;" "<" $ replace "&gt;" ">" html

-- foreign import consoleLog "function consoleLog(x) { console.log(x); return x }" :: forall a. a -> a
-- debug :: forall m. (Monad m) => String -> ParserT String m Unit
-- debug x = do
--   xs <- lookAhead $ char `manyTill` eof
--   let _ = consoleLog $ joinWith "" (x : ":" : xs)
--   return unit
