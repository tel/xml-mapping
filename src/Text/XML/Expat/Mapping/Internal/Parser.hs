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
  LevelState { _namespace  :: Namespace
             , _elemName   :: Text
             , _attributes :: Map.HashMap Namespace (Map.HashMap Text ByteString)
             , _namespaces :: NSMap
             }
makeLenses ''LevelState

-- | As we traverse the XML tree we build a stack of 'LevelState's
-- representing the attribute and element context that we're
-- descending through. This allows for fairly targeted parser error
-- messages.
data LevelSet = Step { _levelState :: LevelState }
              | Root { _levelState :: LevelState }
makeLenses ''LevelSet
makePrisms ''LevelSet

-- | The core parser type. Currently this is implemented as a stack of
-- 'Monad's, but should be CPS transformed eventually. This could also
-- be turned into a transformer itself, but it'd be much nicer to
-- avoid that if possible.
newtype Parser a = P {
  unP :: ReaderT LevelSet (
     ErrorT ParseError (State [Tag])
     ) a
  } deriving ( Functor, Applicative, Alternative )

-- | *Internal.* Execute a 'Parser' in all its gory detail.
runP :: Parser a -> LevelSet -> [Tag] -> (Either ParseError a, [Tag])
runP (P inner) level st = runState (runErrorT (runReaderT inner level)) st

-- | *Internal.* Execute a 'Parser' in the context of another
-- 'Parser'. This might eventually be reasonable to export publically,
-- but right now it's just useful for 'xsOptional' and node descent.
forkP :: Parser a -> Parser (Either ParseError a, [Tag])
forkP parser = P $ ask >>= \level -> get >>= \st -> return (runP parser level st)

-- | XML Simple Types, i.e. types which can be deserialized from plain
-- XML text.
class FromSimpleXMLType a where
  fromSimpleXMLType :: ByteString -> Err String a

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

-- | Parse the value of an XML attribute out of the current parsing
-- context.
attr :: FromSimpleXMLType a => Namespace -> Text -> Parser a
attr ns n = P $ do
  mayBs <- preview (levelState . attributes . ix ns . ix n)
  case mayBs of
    Nothing -> fail $ "No attribute " ++ show (ns, n)
    Just bs -> err fail return $ fromSimpleXMLType bs

attr' :: FromSimpleXMLType a => Text -> Parser a
attr' = attr Free

-- | Check to see whether a particular name matches the current
-- context.

thisElement :: Namespace -> Text -> Parser Bool
thisElement ns name = P $ do
  one <- view $ levelState . namespace . to (== ns)
  two <- view $ levelState . elemName  . to (== name)
  return (one && two)

-- | *Internal.* Concatenates a list of 'Tag's into a single
-- 'ByteString'.
--
-- @
-- >>> collectText []
-- Just ""
-- >>> collectText [Text "foo", Text "bar"]
-- Just "foobar"
-- >>> collectText [Text "foo", Text "bar", Element "foo" [] []]
-- Nothing
-- @
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

-- | *Internal.* When we load into an element we capture the
-- information in the start node into the 'LevelState' in several
-- ways. First, we take the tag name itself and store it in the
-- path. Second, we update the namespace context using any @xmlns@ XML
-- attributes. Thirdly, we update the current attribute context to
-- include all the attributes which remain.

-- updateLevelState :: Tag -> LevelState -> LevelState
-- updateLevelState Text{} ls =  ls
-- updateLevelState
--   (Element { eName       = name
--            , eAttributes = attrs
--            })
--   (LevelState { _path       = path0
--               , _namespaces = ns0
--               }) =
--     let ns1    = updateNS attrs ns0
--         path1  = NEL.cons (getNName ns1 name) path0
--         atmap1 = mkAtMap attrs ns1
--     in  LevelState { _path       = path1
--                    , _namespaces = ns1
--                    , _atmap      = atmap1
--                    }
--   where
--     getNName :: Map.HashMap Text Text
--               -> Text -> (Namespace, Text)
--     getNName nsMap n = case prefix n of
--       Left freeN -> (Free, freeN)
--       Right (tag, nsN) -> (Namespace $ undefined tag nsMap, nsN)
--     updateNS :: [(Text, ByteString)]
--                 -> Map.HashMap Text Text -> Map.HashMap Text Text
--     updateNS = undefined
--     mkAtMap :: [(Text, ByteString)]
--                -> Map.HashMap Text Text
--                -> Map.HashMap Namespace (Map.HashMap Text ByteString)
--     mkAtMap = undefined


-- | Descend into a particular element running a parser.
--
-- The inner parser must fully consume all of the child elements in
-- order to succeed.

-- load :: Parser a -> Tag -> Parser a
-- load _       Text{} = P $ fail "Expecting element, saw text"
-- load parser (Element { eName = name
--                      , eAttributes = attrs
--                      , eChildren = chils
--                      })
--   = P $ do path0       <- view path
--            namespaces0 <- view namespaces
--            atmap0      <- view atmap
--            let path1  = NEL.cons (prefix name) path0
--            let level1 = level0 & path %~
--                                & namespaces %~ updateNS attrs
--                                & atmap .~ mkAtMap attrs
--            case runP parser level1 chils of
--              (Left e,   _)        -> throwError e
--              (Right a, [])        -> return a
--              (_      , leftovers) -> fail ("Did not consume all children: " ++ show leftovers)
--            where
--              updateNS :: [(Text, ByteString)] -> Map.HashMap Text Text -> Map.HashMap Text Text
--              updateNS = undefined
--              mkAtMap :: [(Text, ByteString)] -> Map.HashMap Namespace (Map.HashMap Text ByteString)
--              mkAtMap = undefined
--                --         fromListWith go . map prepare where
--                -- prepare (key, value) = let (ns, name) = prefix key in


-- data ParseError = ParseError { trying :: First String, errors :: [String] }
--                 deriving ( Eq, Ord, Show )

-- instance Monoid ParseError where
--   mempty = ParseError mempty mempty
--   mappend (ParseError t1 e1) (ParseError t2 e2) = ParseError (t1 <> t2) (e1 <> e2)

-- anError :: String -> ParseError
-- anError e = ParseError mempty [e]

-- newtype Parser a = P {
--   unP :: [NNode ByteString]
--          -> Either ParseError
--                    (a, [NNode ByteString])
--   } deriving ( Functor )

-- instance Applicative (Parser) where
--   pure a = P $ \ns -> Right (a, ns)

--   -- This (<*>) instance is slightly better than `ap` in that it'll
--   -- try the second branch even if the first fails and then combine
--   -- the errors. This gives us slightly more comprehensive reporting.

--   P pf <*> P px = P $ \ns -> case pf ns of
--     Left e -> case px ns of
--       Left e' -> Left (e <> e')
--       Right _   -> Left e -- We already failed above
--     Right (f, ns') -> case px ns' of
--       Left e' -> Left e'
--       Right (x, ns'') -> Right (f x, ns'')

-- instance Alternative (Parser) where
--   empty = P go where go _ = Left $ anError "Alternative {empty}"
--   P p1 <|> P p2 = P $ \ns -> case p1 ns of
--     r@Right{} -> r
--     Left e -> case p2 ns of
--       r@Right{} -> r
--       Left e' -> Left (e <> e')

-- instance Monad (Parser) where
--   return = pure
--   P ma >>= f = P $ \ns ->
--     ma ns >>= \(a, ns') ->
--       unP (f a) ns'
--   fail s = P go where go _ = Left (anError s)

-- instance MonadPlus (Parser) where
--   mzero = empty
--   mplus = (<|>)

-- getNs :: Parser [NNode ByteString]
-- getNs = P go where go ns = Right (ns, ns)

-- putNs :: [NNode ByteString] -> Parser ()
-- putNs ns = P go where go _ = Right ((), ns)

-- withNs :: ([NNode ByteString] -> (a, [NNode ByteString])) -> Parser a
-- withNs f = getNs >>= \ns -> let (a, ns') = f ns in putNs ns' >> return a

-- -- | Tries to parse the next element or fails without effect
-- maybe1 :: Parser a -> Parser (Maybe a)
-- maybe1 p = withNs go where
--   go []     = (Nothing, [])
--   go (n:ns) = case parse1 p n of
--     Left pe -> (Nothing, n:ns)
--     Right a -> (Just a, ns)

-- addE :: String -> Parser a
-- addE e = P go where go _ = Left (anError e)

-- failPE :: ParseError -> Parser a
-- failPE pe = P $ \_ -> Left pe

-- -- declare that we're "trying" something
-- declareT :: String -> Parser a
-- declareT t = P go where go _ = Left (ParseError (First (Just t)) mempty)

-- infixl 4 <?>

-- -- How does this interact with the Path? How does it work at all? How
-- -- does Parsec use it?
-- --
-- -- I could implement everything atop a base error throwing function
-- -- like 'ParseError -> Parser n a' which decorates the 'ParseError'
-- -- with the 'Path'.

-- (<?>) :: Parser a -> String -> Parser a
-- p <?> e = p <* addE e

-- try1 :: (NNode ByteString -> Either ParseError a) -> Parser a
-- try1 f = P go where go []     = Left (anError "Elements exhausted")
--                     go (x:xs) = fmap (,xs) (f x)

-- parse1 :: Parser a ->  NNode ByteString -> Either ParseError a
-- parse1 p e = fmap fst (unP p [e])

-- parseM :: Parser a
--          -> [NNode ByteString]
--          -> Either ParseError (a, [NNode ByteString])
-- parseM = unP
