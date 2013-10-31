-- |
-- Module      : Text.XML.Mapping.Internal.Parser
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- .
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- .
-- Low-level definition of the parser.
-- .

module Text.XML.Mapping.Internal.Parser where

import           Control.Applicative
import           Text.XML.Mapping.Schema.Namespace
import           Text.XML.Mapping.Schema.SimpleType

-- | An abstract definition of the parser.
class Alternative p => P p where
  attr :: FromSimple a => QName -> p a
  text :: FromSimple a => p a
  elem :: QName -> p a -> p a
