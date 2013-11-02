{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

-- |
-- Module      : Text.XML.Mapping.Internal.Class
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- .
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- .
-- Abstract definition of the XML representation class.
-- .

module Text.XML.Mapping.Internal.Class where

import           Control.Applicative
import qualified Data.Attoparsec                    as A
import           Data.List.NonEmpty                 (NonEmpty ((:|)))
import           Text.XML.Mapping.Schema.Mixed
import           Text.XML.Mapping.Schema.Namespace
import           Text.XML.Mapping.Schema.SimpleType

-- | An abstract definition of the parser.
class Alternative f => X f where

  -- | Run a parser on an attribute value in the current context.
  pAttr :: A.Parser a -> QName -> f a

  -- | Run a parser on the next text value in the current stream,
  -- consuming it if successful.
  pText :: A.Parser a -> f a

  -- | Run a new 'p'-type parser on the children of the next element,
  -- using the improved element context and consuming the element if
  -- successful.
  pElem :: QName -> f a -> f a

attr :: (FromSimple a, X f) => QName -> f a
attr = pAttr parseSimple

text :: (FromSimple a, X f) => f a
text = pText parseSimple

(#>) :: X f => QName -> f a -> f a
(#>) = pElem

nonEmpty :: X f => f a -> f (NonEmpty a)
nonEmpty p = (:|) <$> p <*> many p

class XML a where
  xml :: forall f . X f => f a

instance XML () where
  xml = pure ()

eitherX :: X f => f a -> f b -> f (Either a b)
eitherX fa fb = Left <$> fa <|> Right <$> fb

mixed :: X f => f a -> f (Mixed a)
mixed pa = Mixed <$> many (eitherX text pa)
