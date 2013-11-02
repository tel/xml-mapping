-- |
-- Module      : Text.XML.Mapping.Schema.Mixed
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- .
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- .
-- XML \"mixed\" content.

module Text.XML.Mapping.Schema.Mixed (

  Mixed (Mixed, unMixed), textOnly, elementsOnly

  ) where

import           Data.Either
import qualified Data.Text   as T

newtype Mixed a = Mixed { unMixed :: [Either T.Text a] }
                deriving ( Show, Eq, Ord )

textOnly :: Mixed a -> [T.Text]
textOnly = lefts . unMixed

elementsOnly :: Mixed a -> [a]
elementsOnly = rights . unMixed
