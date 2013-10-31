{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Text.XML.Expat.Mapping.Internal.Parser where

import           Control.Applicative
import           Control.Error                              hiding (err)
import           Control.Lens
import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Attoparsec.ByteString.Char8           as A8
import           Data.Attoparsec.Number
import           Data.ByteString                            (ByteString)
import           Data.Foldable                              (foldrM)
import qualified Data.HashMap.Strict                        as Map
import           Data.Monoid
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           Data.Text.Encoding                         (decodeUtf8',
                                                             encodeUtf8)
import           Text.XML.Expat.Tree

import           Text.XML.Expat.Mapping.Internal.Namespaces
import           Text.XML.Expat.Mapping.Types

-- | Nothing at all special right now--just collects errors. This
-- would eventually be very useful if it could collect more
-- information about the error context (in the context of @>>@, in the
-- context of @<|>@) and peek ahead to find more errors. See
-- Swierstra, of course.
newtype ParseError = ParseError [String]
                   deriving ( Eq, Show, Ord, Monoid )

instance Error ParseError where
  noMsg    = ParseError ["Error{noMsg}"]
  strMsg s = ParseError [s]

-- | A type synonym over @ErrorT e Identity@ so that we get a free
-- 'Alternative' instance without having to create yet another failure
-- monad.
--
-- TODO: replace this guy with a better error applicative.
type Err e a = ErrorT e Identity a

-- | 'fmap' over an error type in 'Err'.
fmapE :: (e -> e') -> Err e a -> Err e' a
fmapE f (ErrorT mit) = ErrorT $ fmap go mit where
  go (Left e)  = Left (f e)
  go (Right a) = Right a

-- | Eliminator for 'Err'
err :: (e -> c) -> (a -> c) -> Err e a -> c
err f s (ErrorT (Identity it)) = case it of
  Left e  -> f e
  Right a -> s a

-- | The 'LevelState' is the parse-constant contextual state at this
-- \"level\" of the tree. It includes the current location, attributes
-- (metadata) in scope, and a set of currently in-scope namespaces.
data LevelState =
  LevelState { _name       :: NamespaceName
             , _attributes :: Map.HashMap NamespaceName ByteString
             , _namespaces :: NSMap
             }
makeLenses ''LevelState

-- | As we traverse the XML tree we build a stack of 'LevelState's
-- representing the attribute and element context that we're
-- descending through. This allows for fairly targeted parser error
-- messages.
data LevelSet = Step { _levelState :: LevelState, _step :: LevelSet }
              | Root { _levelState :: LevelState }
makeLenses ''LevelSet
makePrisms ''LevelSet

-- | The core parser type. Currently this is implemented as a stack of
-- 'Monad's, but should be CPS transformed eventually. This could also
-- be turned into a transformer itself, but it'd be much nicer to
-- avoid that if possible. In fact, so far it's not even given a
-- 'Monad' instance as it'd be very nice to optimize around the
-- 'Applicative' case if possible.
newtype Parser a = P {
  unP :: ReaderT LevelSet (
     ErrorT ParseError (State [Tag])
     ) a
  } deriving ( Functor, Applicative, Alternative )

-- | /Internal./ Execute a 'Parser' in all its gory detail.
runP :: Parser a -> LevelSet -> [Tag] -> (Either ParseError a, [Tag])
runP (P inner) level st = runState (runErrorT (runReaderT inner level)) st

-- | /Internal./ Execute a 'Parser' in the context of another
-- 'Parser'. This might eventually be reasonable to export publically,
-- but right now it's just useful for 'xsOptional' and node descent.
forkP :: Parser a -> Parser (Either ParseError a, [Tag])
forkP parser = P $ ask >>= \level -> get >>= \st -> return (runP parser level st)

-- | XML Simple Types, i.e. types which can be deserialized from plain
-- XML text.
class FromSimpleXMLType a where
  fromSimpleXMLType :: ByteString -> Err String a

instance FromSimpleXMLType () where
  fromSimpleXMLType _ = pure ()

instance FromSimpleXMLType ByteString where
  fromSimpleXMLType = return

instance FromSimpleXMLType Text where
  fromSimpleXMLType =
    ErrorT . return . fmapL (const "Could not decode UTF-8") . decodeUtf8'

instance FromSimpleXMLType String where
  fromSimpleXMLType = fmap T.unpack . fromSimpleXMLType

-- | @xs:boolean@-style booleans
instance FromSimpleXMLType Bool where
  fromSimpleXMLType t = case t of
    "true"  -> return True
    "1"     -> return True
    "false" -> return False
    "0"     -> return False
    _       -> fail "not a boolean"

instance FromSimpleXMLType Number where
  fromSimpleXMLType bs = case A8.eitherResult (A8.parse A8.number bs `A8.feed` mempty) of
    Left e  -> fail e
    Right a -> return a

instance FromSimpleXMLType Integer where
  fromSimpleXMLType bs = fromSimpleXMLType bs >>= \mayI ->
    case mayI of
      I i -> return i
      D _ -> fail "expecting integer, saw decimal"

instance FromSimpleXMLType Double where
  fromSimpleXMLType bs = fromSimpleXMLType bs >>= \mayD ->
    case mayD of
      I i -> return (fromIntegral i)
      D d -> return d

-- | Space separated list---assumes a UTF-8 encoding
instance FromSimpleXMLType a => FromSimpleXMLType [a] where
  fromSimpleXMLType = mapM (fromSimpleXMLType . encodeUtf8) . T.words <=< fromSimpleXMLType

-- | Convert a parser into one that may fail.
xsOptional :: Alternative f => Parser a -> Parser (f a)
xsOptional parser = P $ do
  (res, leftover) <- unP $ forkP parser
  case res of
    Left  _ -> return empty
    Right a -> put leftover >> return (pure a)

-- | Specializes 'xsOptional'.
xsMaybe :: Parser a -> Parser (Maybe a)
xsMaybe = xsOptional

-- | Defaults for a 'Maybe' parser
xsDefault :: a -> Parser (Maybe a) -> Parser a
xsDefault val p = P $ do
  mayA <- unP p
  case mayA of
    Nothing -> return val
    Just a  -> return a

-- | Matches only when the 'Eq'ual to a sentinel value.
xsFixed :: (Show a, Eq a) => a -> Parser a -> Parser a
xsFixed val p = P $ do
  a <- unP p
  if a == val
    then return a
    else fail ("Did not match fixed value: " ++ show val)

-- | Parse the value of an XML attribute out of the current parsing
-- context.
attr :: FromSimpleXMLType a => NamespaceName -> Parser a
attr nsn = P $ do
  mayBs <- preview (levelState . attributes . ix nsn)
  case mayBs of
    Nothing -> fail $ "No attribute " ++ show nsn
    Just bs -> err fail return $ fromSimpleXMLType bs

-- | Check to see whether a particular name matches the current
-- context.

thisElement :: NamespaceName -> Parser Bool
thisElement nsn = P $ view $ levelState . name . to (== nsn)

-- | /Internal./ Concatenates a list of 'Tag's into a single
-- 'ByteString'.
--
-- >>> collectText []
-- Just ""
--
-- >>> collectText [Text "foo", Text "bar"]
-- Just "foobar"
--
-- >>> collectText [Text "foo", Text "bar", Element "foo" [] []]
-- Nothing
--
collectText :: [Tag] -> Maybe ByteString
collectText = foldr textIt (Just mempty) where
  textIt :: Tag -> Maybe ByteString -> Maybe ByteString
  textIt _          Nothing   = Nothing
  textIt Element{}  _         = Nothing
  textIt (Text bs') (Just bs) = Just (bs <> bs')

-- | Parse XML body text as an XML simple type.
xsSimple :: FromSimpleXMLType a => Parser a
xsSimple = P $ do
  ns <- get
  case collectText ns of
    Nothing -> fail "Expecting text, saw elements"
    Just bs -> err fail finalize (fromSimpleXMLType bs)
  where
    finalize a = put [] >> return a

-- | /Internal./ When we load into an element we capture the
-- information in the start node into the 'LevelState' in several
-- ways.
--
-- 1. We update the namespace context using any @xmlns@ XML
-- attributes.
--
-- 2. We take the tag name itself, resolve it using the new namespace
-- context and store it in the path.
--
-- 3. We build a new attribute context using the update namespace
-- context *and* the new element's namespace in order to resolve
-- defaults.
--
-- The update fails if namespace resolution fails due to an unknown
-- prefix. The 'Left' return is a list of the unresolvable prefixes.
--
-- TODO: Figure out whether this attribute namespace resolution
-- mechanism is actually up to spec. It seems to be the reasonable way
-- that most people interpret and use attribute namespacing, but I
-- would not put it past W3C to have a more bizarre edge case here.
--
-- @
-- newLevelState (Text \"\") == id
-- @
newLevelState :: Tag -> LevelState -> Either [Prefix] LevelState
newLevelState Text{} ls = Right ls
newLevelState
  (Element { eName       = srcName
           , eAttributes = srcAttrs
           })
  (LevelState { _namespaces = nsmap0 }) = do
    let nsmap1 = nsmap0 <> fromAttrs srcAttrs
    name1@(NamespaceName (def, _)) <- resolve nsmap1 srcName

    -- We adjust the default namespace to match the element namespace
    -- while performing attribute namespace resolution.

    attrs1 <- foldrM (mkAttrs (nsmap1 & defaultNS .~ def)) Map.empty srcAttrs
    return LevelState { _name       = name1
                      , _namespaces = nsmap1
                      , _attributes = attrs1
                      }
  where
    mkAttrs :: NSMap
               -> (Text, ByteString)
               -> Map.HashMap NamespaceName ByteString
               -> Either [Prefix] (Map.HashMap NamespaceName ByteString)
    mkAttrs nsmap (key, val) m0 = (\nsn -> Map.insert nsn val m0) <$> resolve nsmap key

-- | Appends a new derived 'LevelState' to the end of a
-- 'LevelSet'. See 'newLevelState'.
appendLevelSet :: Tag -> LevelSet -> Either [Prefix] LevelSet
appendLevelSet t lset = (`Step` lset) <$> newLevelState t (view levelState lset)

-- | Descend into a particular element running a parser.
--
-- The inner parser must fully consume all of the child elements in
-- order to succeed.
load :: Tag -> Parser a -> Parser a
load Text{} _ = P $ fail "Expecting element, saw text"
load tag parser = P $ do
  lSet0 <- ask
  case appendLevelSet tag lSet0 of
    Left pfxs     -> fail $ "Could not resolve these XML prefixes: " ++ show pfxs
    Right lSet1 ->
      case runP parser lSet1 (eChildren tag) of
        (Left e,   _)        -> throwError e
        (Right a, [])        -> return a
        (_      , leftovers) -> fail ("Did not consume all children: " ++ show leftovers)

-- | Descend into the next element so long as it matches, running a
-- parser.
xsElement :: NamespaceName -> Parser a -> Parser a
xsElement nsn parser = P $ do
  es <- get
  case es of
    [] -> fail "Expecting element, but no more remain"
    (e:es') -> unP $ load e $ P $ do
      rightElem <- unP (thisElement nsn)
      if rightElem
        then unP parser <* put es'
        else fail ("Expecting element named " ++ show nsn)

-- | XML Complex Types, i.e. types which are deserialized from some
-- sequence or group of elements.
class FromXML a where
  fromXML :: Parser a

defaultNamespaceMap :: NSMap
defaultNamespaceMap =
  NSMap Free $ Map.fromList [("xmlns", "http://www.w3.org/2000/xmlns/")]

-- | Kick off a parser from scratch.
runParser' :: Parser a -> NSMap -> Tag -> Either ParseError a
runParser' (P go) nsmap tag =
  fst $ flip runState (eChildren tag) $ runErrorT $
  case newLevelState tag (LevelState "" mempty nsmap) of
    Left pfxs -> fail $ "Could not resolve these XML prefixes: " ++ show pfxs
    Right ls0 -> runReaderT go (Root ls0)

runParser :: Parser a -> Tag -> Either ParseError a
runParser p = runParser' p defaultNamespaceMap

instance FromXML () where
  fromXML = pure ()

instance FromXML [Tag] where
  fromXML = P $ state $ \s -> (s, [])

decode :: Parser a -> ByteString -> Either ParseError a
decode (P parser) bs = case parse' defaultParseOptions bs of
  Left e    -> throwError (strMsg $ "Malformed XML: " ++ show e)
  Right tag -> runParser (P $ put [tag] >> parser) tag
