-- |
-- Module      : Text.XML.Mapping.NSMap
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Prefix resolution mapping.

module Text.XML.Mapping.NSMap (

  -- * Core data type
  NSMap (..),

  -- * Name resolution
  resolve,

  -- * Construction and modification
  defaultNSMap, fromAttrs, undeclareNS

  ) where

import qualified Data.ByteString                   as S
import qualified Data.HashMap.Strict               as Map
import           Data.List                         (foldl')
import           Data.Monoid
import qualified Data.Text                         as T
import           Text.XML.Mapping.Schema.Namespace

-- | An 'NSMap' contains all local namespace resolution information.
data NSMap =
  NSMap { defaultNS :: Namespace
          -- ^ The default namespace for un-tagged qualified names
          -- which get resolved to a default (i.e. not attributes
          -- which are maddeningly element-specific in their
          -- behavior).
        , nsMap     :: Map.HashMap Prefix Namespace
        } deriving Show

-- | 'mappend' combines new 'NSMap' information with the \"rightmost\"
-- 'NSMap' winning. This combination is not guaranteed to be a true
-- 'Monoid' except under observation of 'resolve'. Namespace
-- undeclaration is performed by forcing a particular 'Prefix' to map
-- to 'Free'. These \"tombstone\" values may or may not be removed
-- from the resulting map.
instance Monoid NSMap where
  mempty = NSMap Free mempty
  mappend (NSMap _ map1) (NSMap def map2) =
    NSMap def $ Map.unionWith (\_ v2 -> v2) map1 map2

-- | Convert an attribute set to a fresh 'NSMap' representing the
-- *new* information garnered from this particular attribute set.
--
-- This is slightly looser than the actual XML spec demands---it
-- merges multiple declarations of an @xmlns@ attribute \"to the
-- right\".
fromAttrs :: [(T.Text, S.ByteString)] -> NSMap
fromAttrs = foldl' build mempty where
  build nsmap@(NSMap def hmap) (attr, val) =
    case prefix attr of
      Left name | name /= "xmlns" -> nsmap
                | otherwise -> NSMap (Namespace val) hmap
      Right (xmlns, pf)
        | xmlns /= "xmlns" -> nsmap -- what?
        | otherwise ->
          NSMap def $ Map.insert (Prefix $ getLocalName pf)
                                 (Namespace val)
                                 hmap

-- | Whenever we encounter either an @xmlns=\"\"@ or (as of XML
-- Namespaces 1.1) @xmlns:tag=\"\"@ we \"undeclare\" that namespace,
-- i.e. allow it to be resolved upward. 'Free' undeclares the default
-- namespace while 'Namespace' undeclares a particular one.
--
-- @
-- prop> \pf nsmap tn ->
--   realizeNS (undeclareNS pf map) (Right (pf, tn)) == Just (Free, tn)
-- @
undeclareNS :: Maybe Prefix -> NSMap -> NSMap
undeclareNS Nothing   (NSMap _ hm) = NSMap Free hm
undeclareNS (Just pf) (NSMap d hm) = NSMap d $ Map.delete pf hm

-- | Takes the output of 'prefix' to a 'NamespaceName' or fails if it
-- cannot. See 'resolve'.
--
-- If there is no default namespace declaration in scope, the
-- namespace name has no value. The namespace name for an unprefixed
-- attribute name always has no value. In all cases, the local name is
-- local part (which is of course the same as the unprefixed name
-- itself).
realizeNS :: NSMap -> Either LocalName (Prefix, LocalName) -> Either [Prefix] QName
realizeNS nsmap (Left local)     = Right $ QName (defaultNS nsmap) local
realizeNS nsmap (Right (pf, tn)) =
  case Map.lookup pf (nsMap nsmap) of
    Nothing -> Left [pf]
    Just x  -> Right $ QName x tn

-- | Resolves a 'Text' fragment within a 'Namespace' context by
-- treating it as a qualified name and then trying to resolving the
-- namespace prefix in the 'NSMap'. Returns 'Nothing' if the qualified
-- name has an unknown prefix.
resolve :: NSMap -> T.Text -> Either [Prefix] QName
resolve nsmap = realizeNS nsmap . prefix

defaultNSMap :: NSMap
defaultNSMap =
  NSMap Free $ Map.fromList [("xmlns", "http://www.w3.org/2000/xmlns/")]
