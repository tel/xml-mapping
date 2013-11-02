{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Text.XML.Mapping
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- .
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- .

module Text.XML.Mapping (
  decode, Tag,

  module X,
  PE.ParseError

  ) where

import qualified Data.ByteString                      as S
import           Text.XML.Expat.Tree                  hiding (QName)
import           Text.XML.Mapping.Internal.Class      as X
import qualified Text.XML.Mapping.Internal.ParseError as PE
import           Text.XML.Mapping.Internal.Parser
import           Text.XML.Mapping.Schema.Namespace    as X
import           Text.XML.Mapping.Schema.SimpleType   as X
import           Text.XML.Mapping.Types

decode :: XML a => QName -> S.ByteString -> Either PE.ParseError a
decode qn bs = case parse' defaultParseOptions bs of
  Left xpe -> Left $ PE.reason (PE.malformed xpe) PE.at0
  Right n  -> runParser xml qn (Tag n)
