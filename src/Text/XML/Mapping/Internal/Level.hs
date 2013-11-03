-- |
-- Module      : Text.XML.Mapping.Internal.Level
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- .
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- .
-- Fixed contextual state for a particular stage in an XML tree
-- traversal.
-- .
-- Whenever we \"descend\" into a new XML element we add a new step to
-- an implicit path from the root node. We also get a fresh XML
-- Namespace lexical scope and a new attribtue context.
-- .
-- This particular collection of state at a given \"level\" in the XML
-- tree is called the 'Level'. We can also view these states as a
-- stack which gets pushed and popped as we step into and out of
-- elements.

module Text.XML.Mapping.Internal.Level (

  -- * Levels

  Level, level0, nsContext, name, attributes, nextStep, path,
  resolve, step, elemHere, getAttr,

  -- ** Errors
  LevelError (..),

  -- * Supporting types

  AttrMap,

  ) where

import qualified Data.ByteString                   as S
import           Data.Foldable
import qualified Data.HashMap.Strict               as Map
import           Data.List                         (intercalate)
import           Data.Monoid
import qualified Data.Text                         as T
import qualified Text.XML.Mapping.NSMap            as NS
import           Text.XML.Mapping.Schema.Namespace
import           Text.XML.Mapping.Types

-- | A mapping of resolved attribute names.
type AttrMap = Map.HashMap QName S.ByteString

-- | The 'Level' is the contextual information available at a
-- particular \"descent path\" into the XML tree. This information
-- includes name prefix resolution information, the components of the
-- current path, and the most recently uncovered set of attributes.
data Level = Root { nsContext :: !NS.NSMap }
           | Step { nsContext   :: !NS.NSMap
                  , _attributes :: !AttrMap
                  , _name       :: !QName
                  , _nextStep   :: Level
                  }
             deriving ( Eq )

instance Show Level where
  show l = "(Level {{ " ++ intercalate " >> " (map show (path l)) ++ " }})"

name :: Level -> Maybe QName
name Root{} = Nothing
name Step { _name = n } = Just n

attributes :: Level -> AttrMap
attributes Root{} = Map.empty
attributes s      = _attributes s

nextStep :: Level -> Maybe Level
nextStep Root{} = Nothing
nextStep s      = Just (_nextStep s)

-- | Compute the 'path' of 'QName's from the 'Root' of the XML tree to
-- some 'Level'.
path :: Level -> [QName]
path Root {} = []
path Step { _name = n, _nextStep = next } = n : path next

-- | Resolve a raw name in the current context.
resolve :: Level -> T.Text -> Either [Prefix] QName
resolve = NS.resolve . nsContext

getAttr :: Level -> QName -> Maybe S.ByteString
getAttr Root{} _  = Nothing
getAttr s      qn = Map.lookup qn (attributes s)

-- | An initial 'Level' based on no information
level0 :: Level
level0 = Root NS.defaultNSMap

data LevelError = UnresolvedPrefixes [Prefix]
                | SteppingIntoText
                deriving ( Show, Eq, Ord )

fmapL :: (e -> e') -> Either e a -> Either e' a
fmapL f (Left e)  = Left (f e)
fmapL _ (Right a) = Right a

-- | Step into a 'Tag' producing a new, deeper 'Level'.
step :: Tag -> Level -> Either LevelError Level
step t l0 = case rawName t of
  Nothing -> Left SteppingIntoText
  Just qn -> fmapL UnresolvedPrefixes $ do
    let nsmap = nsContext l0 <> NS.fromAttrs (rawAttrs t)
    nm <- NS.resolve nsmap qn

    -- We adjust the default namespace to match the element namespace
    -- while performing attribute namespace resolution.
    attrMap <- foldrM (addAttr $ nsmap { NS.defaultNS = qNamespace nm })
                      Map.empty
                      (rawAttrs t)

    return Step { nsContext   = nsmap
                , _attributes = attrMap
                , _name       = nm
                , _nextStep   = l0
                }

      where
        addAttr nsmap (key, val) m = do
          qualified <- NS.resolve nsmap key
          return $ Map.insert qualified val m

-- | Is this the name of the current element context?
elemHere :: QName -> Level -> Bool
elemHere qn = maybe False (== qn) . name
