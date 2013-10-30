{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Text.XML.Expat.Mapping.Internal.Namespaces where

import           Control.Applicative
import           Control.Lens
import           Data.Hashable
import qualified Data.HashMap.Strict as Map
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

-- | Namespaces on tags. 'Free' implies no namespace (thus \"free\" to
-- take whatever definition the current context might
-- give). 'Namespace' allows injection of a particular fully qualified
-- namespace, not an abbreviation tag as those are never meaningfully
-- canonical.
data Namespace = Free | Namespace Text
               deriving ( Show, Generic )
instance Hashable Namespace

-- | See @Namespaces in XML 1.0: 2.3 Comparing URI References@.
--
-- > The comparison is case-sensitive, and no %-escaping is done or
-- > undone.
--
-- Although, this depends upon the attribute parsing being "nice"
-- according to XML value normalization:
-- <http://www.w3.org/TR/REC-xml/#AVNormalize>. Right now I'm assuming
-- that Expat does this, but it may be completely false.
--
-- TODO: Check that assertion.
instance Eq Namespace where
  Namespace t1 == Namespace t2 = t1 == t2
  Free         == Free         = True
  _            == _            = False

-- A default namespace declaration applies to all unprefixed element
-- names within its scope. Default namespace declarations do not apply
-- directly to attribute names; the interpretation of unprefixed
-- attributes is determined by the element on which they appear.

-- | Empty namespaces (@""@) are interpreted as 'Free'.
instance IsString Namespace where
  fromString [] = Free
  fromString s  = Namespace (fromString s)

-- | Prefixes are short tag-lexically scoped fragments declared by an
-- @xmlns:prefix="namespace"@ attribute.
newtype Prefix = Prefix { getPrefix :: Text }
               deriving ( Show, Eq, Ord, Hashable, IsString )

-- | 'Tagname's are "prefix-free" names. Technically they ought to be
-- XML @NCName@s but rather than rejecting the parse entirely, they're
-- allowed to be created including colons so long as the 'Prefix' is
-- only \"pulled-off\" once.
newtype Tagname = Tagname { getTagName :: Text }
                deriving ( Show, Eq, Ord, Hashable, IsString )

-- | A 'NamespaceName' just refers to the product of a full
-- 'Namespace' (XML "namespace name\") and a 'Tagname' (XML \"local
-- name\").
type NamespaceName = (Namespace, Tagname)

-- | Pull a, presumably *qualified*, name apart into its prefix and
-- its body. Technically this should ensure that any 'Namespace' is an
-- @NCName@, but right now it just pull off any chunk before the first
-- colon.
--
-- TODO: Make this smarter.
--
-- @
-- >>> prefix "foo"
-- (Free, "foo")
-- >>> prefix "foo:bar"
-- (Namespace "foo", "bar")
-- @
--
prefix :: Text -> Either Tagname (Prefix, Tagname)
prefix t = case T.split (==':') t of
  []     -> Left (Tagname T.empty)
  [n]    -> Left (Tagname n)
  (n:ns) -> Right (Prefix n, Tagname $ T.concat ns)

-- | An 'NSMap' contains all local namespace resolution information.
data NSMap =
  NSMap { _defaultNS :: Namespace
          -- ^ The default namespace for un-tagged qualified names
          -- which get resolved to a default (i.e. not attributes
          -- which are maddeningly element-specific in their
          -- behavior).
        , _nsMap     :: Map.HashMap Prefix Namespace
        }
makeLenses ''NSMap

-- | Whenever we encounter either an @xmlns=""@ or (as of XML
-- Namespaces 1.1) @xmlns:tag=""@ we \"undeclare\" that namespace,
-- i.e. allow it to be resolved upward. 'Free' undeclares the default
-- namespace while 'Namespace' undeclares a particular one.
--
-- @
-- prop> \pf nsmap tn ->
--   realizeNS (undeclareNS pf map) (Right (pf, tn)) == Just (Free, tn)
-- @
undeclareNS :: Maybe Prefix -> NSMap -> NSMap
undeclareNS Nothing   nsmap = nsmap & defaultNS     .~ Free
undeclareNS (Just pf) nsmap = nsmap & nsMap . at pf .~ Nothing

-- | Takes the output of 'prefix' to a 'NamespaceName' or fails if it
-- cannot. See 'resolve'.
--
-- If there is no default namespace declaration in scope, the
-- namespace name has no value. The namespace name for an unprefixed
-- attribute name always has no value. In all cases, the local name is
-- local part (which is of course the same as the unprefixed name
-- itself).
realizeNS :: NSMap -> Either Tagname (Prefix, Tagname) -> Maybe NamespaceName
realizeNS nsmap (Left tagname)   = Just (nsmap ^. defaultNS, tagname)
realizeNS nsmap (Right (pf, tn)) = (, tn) <$> preview (nsMap . ix pf) nsmap

-- | Resolves a 'Text' fragment within a 'Namespace' context by
-- treating it as a qualified name and then trying to resolving the
-- namespace prefix in the 'NSMap'. Returns 'Nothing' if the qualified
-- name has an unknown prefix.
resolve :: NSMap -> Text -> Maybe NamespaceName
resolve nsmap = realizeNS nsmap . prefix
