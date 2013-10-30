
module Text.XML.Expat.Mapping.Types where

import           Data.ByteString     (ByteString)
import           Data.Text           (Text)
import           Text.XML.Expat.Tree

-- | This is a very simple initial parse of the @hexpat@ tree. We
-- don't gather 'QName's and 'NName's because that requires extra tree
-- traversals and our parser will be collecting that information
-- anyway. Tags are collected as 'Text' because I assume that they can
-- be split around @':'@, but the main text is left as 'ByteString' to
-- allow for bizarre things in the CDATA.
type Tag = Node Text ByteString
