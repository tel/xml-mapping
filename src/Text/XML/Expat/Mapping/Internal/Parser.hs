{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Text.XML.Expat.Mapping.Internal.Parser where

import           Control.Error                    hiding (err)
import           Control.Lens
import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Attoparsec.ByteString.Char8 as A8
import           Data.Attoparsec.Number
import           Data.ByteString                  (ByteString)
import           Data.Hashable
import qualified Data.HashMap.Strict              as Map
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8', encodeUtf8)
import           GHC.Generics
import           Text.XML.Expat.Tree

data Namespace = Free | Namespace Text
               deriving ( Show, Eq, Ord, Generic )
instance Hashable Namespace

newtype ParseError = ParseError [String]
                   deriving ( Eq, Show, Ord, Monoid )

instance Error ParseError where
  noMsg    = ParseError ["Error{noMsg}"]
  strMsg s = ParseError [s]

type Err e a = ErrorT e Identity a

fmapE :: Functor m => (e -> e') -> ErrorT e m a -> ErrorT e' m a
fmapE f (ErrorT mit) = ErrorT $ fmap go mit where
  go (Left e)  = Left (f e)
  go (Right a) = Right a

err :: (e -> c) -> (a -> c) -> Err e a -> c
err f s (ErrorT (Identity it)) = case it of
  Left e  -> f e
  Right a -> s a

data LevelState =
  LS { _path  :: [(Namespace, Text)]
     , _atmap :: Map.HashMap Namespace (Map.HashMap ByteString ByteString)
     , _ctx   :: Map.HashMap Text Text
     }
makeLenses ''LevelState

newtype Parser a = P {
  unP :: ReaderT LevelState (
     ErrorT ParseError (State [NNode ByteString])
     ) a
  }

class FromXMLAttribute a where
  fromXMLAttribute :: ByteString -> Err String a

instance FromXMLAttribute ByteString where
  fromXMLAttribute = return

instance FromXMLAttribute Text where
  fromXMLAttribute =
    ErrorT . return . fmapL (const "Could not decode UTF-8") . decodeUtf8'

-- | @xs:boolean@-style booleans
instance FromXMLAttribute Bool where
  fromXMLAttribute t = case t of
    "true"  -> return True
    "1"     -> return True
    "false" -> return False
    "0"     -> return False
    _       -> fail "not a boolean"

instance FromXMLAttribute Number where
  fromXMLAttribute bs = case A8.eitherResult (A8.parse A8.number bs `A8.feed` mempty) of
    Left e  -> fail e
    Right a -> return a

instance FromXMLAttribute Integer where
  fromXMLAttribute bs = fromXMLAttribute bs >>= \mayI ->
    case mayI of
      I i -> return i
      D _ -> fail "expecting integer, saw decimal"

instance FromXMLAttribute Double where
  fromXMLAttribute bs = fromXMLAttribute bs >>= \mayD ->
    case mayD of
      I i -> return (fromIntegral i)
      D d -> return d

instance FromXMLAttribute a => FromXMLAttribute (Maybe a) where
  fromXMLAttribute = return . err (const Nothing) Just . fromXMLAttribute

-- | Space separated list---assumes a UTF-8 encoding
instance FromXMLAttribute a => FromXMLAttribute [Maybe a] where
  fromXMLAttribute = mapM (fromXMLAttribute . encodeUtf8) . T.words <=< fromXMLAttribute

attr :: FromXMLAttribute a => Namespace -> ByteString -> Parser a
attr ns n = P $ do
  mayBs <- preview (atmap . ix ns . ix n)
  case mayBs of
    Nothing -> fail $ "No attribute " ++ show (ns, n)
    Just bs -> err fail return $ fromXMLAttribute bs

attr' :: FromXMLAttribute a => ByteString -> Parser a
attr' = attr Free

-- runParser :: Parser a -> Namespace -> ByteString -> UNode ByteString -> Either ParseError a
-- runParser _      _  _ Text{} = fail "Expecting element, saw text"
-- runParser (P go) ns n
--   (Element { eName = name
--            , eAttributes = attrs
--            , eChildren = chils
--            }) = P $ do
--     ctx <- view _1
--     case ns of
--       Free ->

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
