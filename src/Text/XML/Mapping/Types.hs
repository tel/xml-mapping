-- |
-- Module      : Text.XML.Mapping.Types
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- .
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- .
-- Globally relevant types for XML mapping. Primarily used to create
-- an abstract, simplified veneer over the @hexpat@ types so that they
-- can eventually be replaced.

module Text.XML.Mapping.Types (

  Tag (..), children, rawAttrs, rawName, rawText, ignorable

  ) where

import qualified Data.Attoparsec.Char8 as A8
import qualified Data.ByteString       as S
import           Data.Char             (isSpace)
import qualified Data.Text             as T
import           Text.XML.Expat.Tree


-- | This is a very simple initial parse of the @hexpat@ tree. We
-- don't gather 'QName's and 'NName's because that requires extra tree
-- traversals and our parser will be collecting that information
-- anyway. Tags are collected as 'Text' because I assume that they can
-- be split around @':'@, but the main text is left as 'ByteString' to
-- allow for bizarre things in the CDATA.

newtype Tag = Tag { unTag :: Node T.Text S.ByteString }

-- | If this is a text node then there are no children.
children :: Tag -> [Tag]
children (Tag Text{}) = []
children (Tag e)      = map Tag (eChildren e)

-- | If this is a text node then there are no attributes.
rawAttrs :: Tag -> [(T.Text, S.ByteString)]
rawAttrs (Tag Text{}) = []
rawAttrs (Tag e)      = eAttributes e

-- | If this is a text node then there the name is 'Nothing'.
rawName :: Tag -> Maybe T.Text
rawName (Tag Text{}) = Nothing
rawName (Tag e)      = Just (eName e)

rawText :: Tag -> Maybe S.ByteString
rawText (Tag (Text t)) = Just t
rawText _              = Nothing

-- | Determines whether this tag is ignorable. Essentially this only
-- occurs if it's an "empty text" tag---either complete empty or only
-- spaces.
ignorable :: Tag -> Bool
ignorable (Tag (Text t))
  | S.null t  = True
  | otherwise =
    let isRight Left{} = False
        isRight _      = True
    in isRight $ A8.parseOnly (A8.takeWhile isSpace >> A8.endOfInput) t
ignorable _              = False
