-- |
-- Module      : Text.XML.Mapping.Internal.LevelSet
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Fixed contextual state for a particular stage in an XML tree
-- traversal.
--
-- Whenever we \"descend\" into a new XML element we add a new step to
-- an implicit path from the root node. We also get a fresh XML
-- Namespace lexical scope and a new attribtue context.
--
-- This particular collection of state at a given \"level\" in the XML
-- tree is called the 'LevelState'. We can also view these states as a
-- stack which gets pushed and popped as we step into and out of
-- elements called a 'LevelSet'.

module Text.XML.Mapping.Internal.LevelSet where

import qualified Data.ByteString                   as S
import qualified Data.HashMap.Strict               as Map
import           Text.XML.Expat.Mapping.Types
import           Text.XML.Mapping.NSMap
import           Text.XML.Mapping.Schema.Namespace

type AttrMap = Map.HashMap QName S.ByteString

-- | The 'LevelState' is the parse-constant contextual state at this
-- \"level\" of the tree. It includes the current location, attributes
-- (metadata) in scope, and a set of currently in-scope namespaces.
data LevelState =
  LevelState { name       :: !QName
             , attributes :: !AttrMap
             , namespaces :: !NSMap
             }

-- | Kick off a 'LevelSet' from element data.
initialize :: Tag -> LevelSet
initialize = undefined

-- | As we traverse the XML tree we build a stack of 'LevelState's
-- representing the attribute and element context that we're
-- descending through. This allows for fairly targeted parser error
-- messages.
--
-- To be more clear, this is isomorphic to `NonEmpty LevelSet`.
data LevelSet = In   { levelState :: !LevelState, _out :: !(LevelSet) }
              | Root { levelState :: !LevelState }

-- | Safe '_out'.
out :: LevelSet -> Maybe LevelSet
out i@In{} = Just (_out i)
out _      = Nothing
