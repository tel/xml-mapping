{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Module      : Text.XML.Mapping.Schema.Namespace
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- .
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- .
-- Namespace and name types for XML.

module Text.XML.Mapping.Schema.Namespace (

  -- * Namespace types
  Namespace (..), Prefix (..), LocalName (..), QName (..),

  -- ** Constructor functions
  prefix, (-:), inNS

  ) where

import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S8
import           Data.Hashable
import           Data.String
import qualified Data.Text             as T
import           GHC.Generics          (Generic)

-- | Namespaces on tags. 'Free' implies no namespace (thus \"free\" to
-- take whatever definition the current context might
-- give). 'Namespace' allows injection of a particular fully qualified
-- namespace, not an abbreviation tag as those are never meaningfully
-- canonical.
data Namespace = Free | Namespace S.ByteString
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

-- | Empty namespaces like @\"\"@ are interpreted as 'Free'.
instance IsString Namespace where
  fromString [] = Free
  fromString s  = Namespace (fromString s)

-- | Prefixes are short tag-lexically scoped fragments declared by an
-- @xmlns:prefix="namespace"@ attribute.
newtype Prefix = Prefix { getPrefix :: T.Text }
               deriving ( Show, Eq, Ord, Hashable, IsString )

-- | 'LocalName's are "prefix-free" names. Technically they ought to be
-- XML @NCName@s but rather than rejecting the parse entirely, they're
-- allowed to be created including colons so long as the 'Prefix' is
-- only \"pulled-off\" once.
newtype LocalName = LocalName { getLocalName :: T.Text }
                  deriving ( Show, Eq, Ord, Hashable, IsString )

-- | A 'QName' just refers to the product of a full 'Namespace' and a
-- 'LocalName'.
data QName = QName { qNamespace :: Namespace, qLocalName :: LocalName }
                      deriving ( Eq, Generic )
instance Hashable QName

-- | Clark notation: @{namespace}name@ with 'Free' being @{}@.
instance Show QName where
  show (QName Free (LocalName tag))           = "{}" ++ T.unpack tag
  show (QName (Namespace ns) (LocalName tag)) =
    "{" ++ S8.unpack ns ++ "}" ++ T.unpack tag

-- | This can only be used to fix local part of a
-- 'NamespaceName'. It's a runtime error to use "fromString" to create
-- a 'NamespaceName' that includes a colon.
instance IsString QName where
  fromString s = case prefix (fromString s) of
    Left tn -> QName Free tn
    Right _ ->
      error $ "Cannot interpret " ++ show s ++ " as an XML local name"

-- | Pull a, presumably /qualified/, name apart into its prefix and
-- its body. Technically this should ensure that any 'Namespace' is an
-- @NCName@, but right now it just pulls off all the chunks prior to
-- the final colon and calls them the namespace in order to create a
-- \"non-colonized\" 'LocalName'.
--
-- TODO: Make this smarter.
--
-- >>> prefix "foo"
-- Left (LocalName {getLocalName = "foo"})
--
-- >>> prefix "foo:bar"
-- Right (Prefix {getPrefix = "foo"},LocalName {getLocalName = "bar"})
--
-- >>> prefix "foo:bar:baz"
-- Right (Prefix {getPrefix = "foo:bar"},LocalName {getLocalName = "baz"})
--
prefix :: T.Text -> Either LocalName (Prefix, LocalName)
prefix t = case T.split (==':') t of
  []     -> Left (LocalName T.empty)
  [n]    -> Left (LocalName n)
  ns     ->
    let (local:revNs) = reverse ns
    in  Right (Prefix . T.intercalate ":" $ reverse revNs, LocalName local)

-- | Set the 'Namespace' of a particular 'NamespaceName'.
(-:) :: Namespace -> QName -> QName
(-:) = inNS

-- | Set the 'Namespace' of a particular 'NamespaceName'.
inNS :: Namespace -> QName -> QName
inNS ns (QName _ t) = QName ns t
