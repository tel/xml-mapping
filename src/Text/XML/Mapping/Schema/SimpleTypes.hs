{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Text.XML.Mapping.Schema.SimpleTypes
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Parsers for XML @xs:simpleType@ data mapping.
-- .
-- XML attributes and text elements are parsed as @xs:simpleType@s.
-- .
-- No instance for 'String' is available (though it's easy to generate
-- it yourself using 'T.unpack') so that we don't have to use the
-- 'showList' trick to enable @[a]@ to mean an XML, space-separated
-- list.

-- Notes

-- * The whitespace facet should be chosen with robust defaults. This
--   behavior is a bit strange for 'S.ByteString' parsers though since
--   they'll have the initial whitespace skipped but the final
--   whitespace retained. The alternative would require a traversal
--   over every 'S.ByteString' parsed.

module Text.XML.Mapping.Schema.SimpleTypes (

  -- * Parser class
  FromSimple (..),

  -- * XML Schema Simple Data Types
  XSBase64Binary (..), XSHexBinary (..)

  ) where

import           Control.Applicative
import qualified Data.Attoparsec                   as A
import qualified Data.Attoparsec.Char8             as A8
import qualified Data.ByteString                   as S
import qualified Data.ByteString.Base16            as S16
import qualified Data.ByteString.Base64            as S64
import           Data.Char                         (isSpace)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE

import           Text.XML.Mapping.NSMap
import           Text.XML.Mapping.Schema.Namespace

{- ========= Basic Instances ========= -}

-- | Types which instantiate 'FromSimple' can be interpreted from XML
-- strings found in attributes or text elements. Minimal complete
-- definition is 'parseSimple' or 'parseSimple''.
class FromSimple a where
  parseSimple :: A.Parser a
  parseSimple = parseSimple' defaultNSMap

  -- | Sometimes simple attributes cannot be parsed unless we can
  -- resolve prefixes. By default this extra information is ignored.
  parseSimple' :: NSMap -> A.Parser a
  parseSimple' _ = parseSimple

instance FromSimple () where
  parseSimple = pure ()

-- | Parses a non-empty 'S.ByteString'
instance FromSimple S.ByteString where
  parseSimple = A.takeWhile1 (const True)

-- | Parses non-empty 'T.Text'
instance FromSimple T.Text where
  parseSimple =
    parseSimple >>= either decodeError return . TE.decodeUtf8'
    where decodeError de = fail ("Unicode decoding failure: " ++ show de)

-- | Matches @xs:boolean@-style booleans
instance FromSimple Bool where
  parseSimple =
    A.choice [ A.string "true"  *> pure True
             , A.string "1"     *> pure True
             , A.string "false" *> pure False
             , A.string "0"     *> pure False
             ]

instance FromSimple A8.Number where
  parseSimple = A8.number

instance FromSimple Integer where
  parseSimple = parseSimple >>= tryI where
    tryI (A8.I i) = return i
    tryI (A8.D _) = fail "Expecting xs:integer, saw a xs:double"

instance FromSimple Int where
  parseSimple = parseSimple >>= tryI where
    tryI (A8.I i) = return (fromIntegral i)
    tryI (A8.D _) = fail "Expecting xs:integer, saw a xs:double"

instance FromSimple Double where
  parseSimple = parseSimple >>= tryD where
    tryD (A8.I i) = return (fromIntegral i)
    tryD (A8.D d) = return d

instance FromSimple Float where
  parseSimple = fromRational . toRational <$> (parseSimple :: A.Parser Double)

-- | Space separated XML list---first parses each component as an
-- 'XSWord' then subparses on each 'XSWord'.
instance FromSimple a => FromSimple [a] where
  parseSimple = many parseWord where
    parseWord = do
      bs <- A.choice [ A8.takeWhile1 isSpace *> A8.takeWhile1 (not . isSpace)
                     ,                          A8.takeWhile1 (not . isSpace)
                     ]
      case A.parseOnly parseSimple bs of
        Left pe -> fail (show pe)
        Right a -> return a

instance (FromSimple a, FromSimple b) => FromSimple (Either a b) where
  parseSimple = (Left <$> parseSimple) <|> (Right <$> parseSimple)

{- ========= XSD Types ========= -}

instance FromSimple QName where
  parseSimple' nsmap = do
    t <- parseSimple -- parse Text
    case resolve nsmap t of
      Left pfxs ->
        fail ("Namespace resolution failure, could not resolve the prefixes " ++ show pfxs)
      Right qn  -> return qn

newtype XSBase64Binary = XSBase64Binary { getBase64Binary :: S.ByteString }
                       deriving ( Show, Eq, Ord )

instance FromSimple XSBase64Binary where
  parseSimple = do
    bs <- parseSimple
    -- Should we use the URL-safe form of this?
    case S64.decode bs of
      Left  e -> fail ("Base 64 decoding failure: " ++ show e)
      Right s -> return (XSBase64Binary s)

newtype XSHexBinary = XSHexBinary { getHexBinary :: S.ByteString }
                       deriving ( Show, Eq, Ord )

instance FromSimple XSHexBinary where
  parseSimple = do
    bs <- parseSimple
    case S16.decode bs of
      (res, leftover)
        | S.null leftover -> return (XSHexBinary res)
        | otherwise       ->
          fail ("Hexadecimal decoding failure, leftovers are: " ++ show leftover)

{- ========= Dev Tools ========= -}

-- | Adjust a simple parser to trim 'isSpace' bytes off each end of
-- the string (if possible).
trimEnds :: A.Parser a -> A.Parser a
trimEnds p = A8.takeWhile isSpace *> p <* A8.takeWhile isSpace

_trySimple :: (FromSimple a, Show a) => S.ByteString -> IO a
_trySimple = go . A.parseOnly (trimEnds parseSimple <* A.endOfInput) where
  go (Left er) = error (show er)
  go (Right a) = print a >> return a
