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
import Data.Array (many)
import Data.List (toUnfoldable)

import Control.Alt
-- import Control.Monad.Error(strMsg)

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String
import Prelude hiding (between)

parse :: String -> Either ParseError Module
parse input = runParser input pModule

newtype Module = Module (Array Declaration)

newtype Declaration = Declaration { rawCode :: String
                                  , body :: Node }

instance eqModule :: Eq Module where
  eq (Module a) (Module a') = a == a'

instance eqDeclaration :: Eq Declaration where
  eq (Declaration a) (Declaration a') = a.rawCode == a'.rawCode && a.body == a'.body

pModule :: forall m. (Monad m) => ParserT String m Module
pModule = do
  skipEmptyLines
  decs <- many pDeclaration
  eof
  pure $ Module decs

pIndent  :: forall m. (Monad m) => ParserT String m Unit
pIndent = void $ oneOf [' ', '\t']

skipEmptyLines :: forall m. (Monad m) => ParserT String m Unit
skipEmptyLines = do
  skipMany $ try emptyLine
  where emptyLine = do
          many pIndent
          oneOf ['\n', '\r']
          pure unit

pDeclaration :: forall m. (Monad m) => ParserT String m Declaration
pDeclaration = do
  rawCodes <- line `many1Till` lookAhead pIndent
  body <- pNode
  skipEmptyLines
  pure $ Declaration { rawCode: joinWith "" $ toUnfoldable rawCodes, body: body }

line :: forall m. (Monad m) => ParserT String m String
line = do
  l <- stringTill $ string "\n"
  pure $ l <> "\n"

data Node = ElementNode TagName (Array Attribute) (Array Node)
          | TextNode String
          | RawTextNode (Array HString)
          | NodeExp HExp

instance eqNode :: Eq Node where
  eq (ElementNode a b c) (ElementNode a' b' c') = a == a' && b == b' && c == c'
  eq (TextNode a) (TextNode a') = a == a'
  eq (RawTextNode a) (RawTextNode a') = a == a'
  eq (NodeExp a) (NodeExp a') = a == a'
  eq _ _ = false

pNode :: forall m. (Monad m) => ParserT String m Node
pNode = try pNodeExp <|> try pElementNode <|> pTextNode

pNodeExp :: forall m. (Functor m, Monad m) => ParserT String m Node -- NodeExp
pNodeExp = do
  skipSpaces
  NodeExp <$> pHExp

pTextNode :: forall m. (Functor m, Monad m) => ParserT String m Node -- TextNod
pTextNode = do
  s <- string1Till $ lookAhead $ ((void $ string "<") <|> eof)
  pure $ TextNode $ unescapeHtml s

pElementNode :: forall m. (Monad m) => ParserT String m Node -- ElementNode
pElementNode = (try pVoidElement) <|> (try pRawElement) <|> (try pEscapableRawElement) <|> pNormalElement

pVoidElement :: forall m. (Monad m) => ParserT String m Node -- ElementNode
pVoidElement = do
  skipSpaces
  Tuple tag attrs <- pStartTagVoid
  pure $ ElementNode tag attrs []
  where pStartTagVoid = pStartTag (pTagNameOneOf voidElementTags end) true
        end = whiteSpace1 <|> (lookAhead $ choice $ map string [">", "/>"])

voidElementTags :: Array TagName
voidElementTags = ["area", "base", "br", "col", "embed", "hr", "img", "input", "keygen", "link", "menuitem", "meta", "param", "source", "track", "wbr"]

pRawElement :: forall m. (Monad m) => ParserT String m Node -- ElementNode
pRawElement = do
  skipSpaces
  Tuple tag attrs <- pStartTagRaw
  children <- pRawTextNode (lookAhead $ pEndTag tag) `manyTill` (try $ pEndTag tag)
  pure $ ElementNode tag attrs $ toUnfoldable children
  where pStartTagRaw = pStartTag (pTagNameOneOf rawElementTags end) false
        end = whiteSpace1 <|> (lookAhead $ string ">")

rawElementTags :: Array TagName
rawElementTags = ["script", "style"]

pEscapableRawElement :: forall m. (Monad m) => ParserT String m Node -- ElementNode
pEscapableRawElement = do
  skipSpaces
  Tuple tag attrs <- pStartTagEscapableRaw
  children <- pRawTextNode (lookAhead $ pEndTag tag) `manyTill` (try $ pEndTag tag)
  pure $ ElementNode tag attrs $ toUnfoldable children
  where pStartTagEscapableRaw = pStartTag (pTagNameOneOf escapableRawElementTags end) false
        end = whiteSpace1 <|> (lookAhead $ string ">")

escapableRawElementTags :: Array TagName
escapableRawElementTags = ["textarea", "title"]

pRawTextNode :: forall a m. (Functor m, Monad m) => ParserT String m a -> ParserT String m Node -- TextNod
pRawTextNode end = do
  RawTextNode <$> pHStrings1 end

pNormalElement :: forall m. (Monad m) => ParserT String m Node -- ElementNode
pNormalElement = do
  skipSpaces
  Tuple tag attrs <- pStartTag (pTagName end) false
  children <- pNode `manyTill` (try $ pEndTag tag)
  pure $ ElementNode tag attrs $ toUnfoldable children
  where end = whiteSpace1 <|> (lookAhead $ string ">") -- XXX ? "/>"

pStartTag :: forall m. (Monad m) => ParserT String m TagName -> Boolean -> ParserT String m (Tuple TagName (Array Attribute))
pStartTag pTagName allowSlash = do
  skipSpaces
  string "<"
  tag <- pTagName
  attributes <- pAttributes (lookAhead end)
  skipSpaces
  end
  pure $ Tuple tag attributes
  where
    end = do
      skipSpaces
      if allowSlash
        then choice $ map string [">", "/>"]
        else string ">"

pEndTag :: forall m. (Monad m) => TagName -> ParserT String m Unit
pEndTag tag = do
  skipSpaces
  string $ joinWith "" [ "</", tag, ">" ]
  pure unit

type TagName = String

pTagName :: forall a m. (Monad m) => ParserT String m a -> ParserT String m TagName
pTagName end = do
  skipSpaces
  string1Till end

pTagNameOneOf :: forall a m. (Monad m) => (Array TagName) -> ParserT String m a -> ParserT String m TagName
pTagNameOneOf tags end = do
  skipSpaces
  tag <- choice $ map string tags
  end
  pure tag

pAttributes :: forall a m. (Monad m) => ParserT String m a -> ParserT String m (Array Attribute)
pAttributes end = toUnfoldable <$> pAttribute (lookAhead $ end) `manyTill` (try end)

pAttributesEnd  :: forall m. (Monad m) => ParserT String m Unit
pAttributesEnd = lookAhead do
  skipSpaces
  string ">" <|> string "/>"
  pure unit

-- embedded purescript expiression
data Attribute = Attr AttributeName AttributeValue
               | Toggle AttributeName
               -- | Style [CSSProperty] --TODO: implement
               -- | Class [ClassName] --TODO: implement
               | AttributesExp HExp

instance eqAttribute :: Eq Attribute where
  eq (Attr a b) (Attr a' b') = a == a' && b == b'
  eq (Toggle a) (Toggle a') = a == a'
  eq (AttributesExp a) (AttributesExp a') = a == a'
  eq _ _ = false

pAttribute :: forall a m. (Monad m) => ParserT String m a -> ParserT String m Attribute
pAttribute attrsEnd = do
  skipSpaces
  try pAttributesExp <|> try (pAttr end) <|> pToggle end
  where
    end = (void $ whiteSpace1) <|> (void attrsEnd)

pAttributesExp :: forall m. (Functor m, Monad m) => ParserT String m Attribute
pAttributesExp = AttributesExp <$> pHExp

pAttr :: forall a m. (Monad m) => ParserT String m a -> ParserT String m Attribute
pAttr end = do
  name <- pAttributeName $ string "="
  value <- pAttributeValue end
  pure $ Attr name value

pToggle :: forall a m. (Monad m) => ParserT String m a -> ParserT String m Attribute -- Toggle
pToggle end = do
  name <- pAttributeName end
  pure $ Toggle name

type AttributeName = String

pAttributeName :: forall a m. (Monad m) => ParserT String m a -> ParserT String m AttributeName
pAttributeName end = unescapeHtml <$> string1Till end

type AttributeValue = Array HString

pAttributeValue :: forall a m. (Monad m) => ParserT String m a -> ParserT String m AttributeValue
pAttributeValue end =
  (try (pDoubleQuotedHStrings >>= (end $> _))) <|>
  (try (pSingleQuotedHStrings >>= (end $> _))) <|>
  pHStrings1 end  -- XXX should not contain quotes

pSingleQuotedHStrings :: forall m. (Monad m) => ParserT String m (Array HString)
pSingleQuotedHStrings = between sq sq $ pHStrings1 (lookAhead sq)
  where sq = string "'"

pDoubleQuotedHStrings :: forall m. (Monad m) => ParserT String m (Array HString)
pDoubleQuotedHStrings = between dq dq $ pHStrings1 (lookAhead dq)
  where dq = string "\""

newtype HExp = HExp String
instance eqHExp :: Eq HExp where
  eq (HExp a) (HExp a') = a == a'

pHExp :: forall m. (Monad m) => ParserT String m HExp
pHExp = do
  string "<%"
  body <- string1Till $ string "%>"
  pure $ HExp body

data HString = StringLiteral String | StringExp HExp

instance eqHString :: Eq HString where
  eq (StringLiteral a) (StringLiteral a') = a == a'
  eq (StringExp a) (StringExp a') = a == a'
  eq _ _ = false

pHStrings1 :: forall a m. (Monad m) => ParserT String m a -> ParserT String m (Array HString)
pHStrings1 end = toUnfoldable <$> (pHString $ lookAhead ((try $ void $ string "<%") <|> (void end))) `many1Till` (try end)

pHString :: forall a m. (Functor m, Monad m) => ParserT String m a -> ParserT String m HString
pHString end = (StringExp <$> pHExp) <|> (StringLiteral <<< unescapeHtml <$> string1Till end)

stringTill :: forall a m. (Functor m, Monad m) => ParserT String m a -> ParserT String m String
stringTill end =
  (fromCharArray <<< toUnfoldable) <$> (anyChar `manyTill` (try end))

string1Till :: forall a m. (Functor m, Monad m) => ParserT String m a -> ParserT String m String
string1Till end =
  (fromCharArray <<< toUnfoldable) <$> (anyChar `many1Till` (try end))

whiteSpace1 :: forall m. (Functor m, Monad m) => ParserT String m String
whiteSpace1 = do
  let space = oneOf ['\n', '\r', ' ', '\t']
  x <- space
  xs <- whiteSpace
  pure $ (singleton x) <> xs

-- stringi :: forall a m. (Monad m) => String -> ParserT String m String
-- stringi s = ParserT $ \s' ->
--   pure $ case toLower $ take (length s) s' of
--     s'' | s'' == toLower s -> { consumed: true, input: drop (length s) s', result: Right s }
--     _ -> { consumed: false, input: s', result: Left (strMsg ("Expected " ++ show s)) }

unescapeHtml :: String -> String
unescapeHtml html = replace (Pattern "&amp;") (Replacement "&") $
                    replace (Pattern "&lt;")  (Replacement "<") $
                    replace (Pattern "&gt;")  (Replacement ">") html

-- debug :: forall m. (Monad m) => String -> ParserT String m Unit
-- debug x = do
--   xs <- lookAhead $ char `manyTill` eof
--   let _ = consoleLog $ joinWith "" (x : ":" : xs)
--   pure unit

foreign import log :: forall a. a -> a
