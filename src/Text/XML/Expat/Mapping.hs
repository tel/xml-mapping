{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- Deficiencies
--
-- It's easy to try to parse a "type" when you want an element, though
-- XML does not fix the name of an type's element.
--
-- node should be xsElement
--
-- A One-style parser should be able to pick up the attributes in any
-- order (or at least in a specific order)

module Text.XML.Expat.Mapping where

import           Text.XML.Expat.Mapping.Internal.Parser
import           Text.XML.Expat.Tree


-- xsSequence :: Parser a -> Parser [a]
-- xsSequence parser = withNs tryEm
--   where
--     tryEm = tryEm' []
--     tryEm' as []     = (reverse as, [])
--     tryEm' as (n:ns) = case parse1 parser n of
--       Left _  -> (reverse as, n:ns)
--       Right a -> tryEm' (a:as) ns

-- instance FromXML a => FromXML (NonEmpty a) where
--   fromXML = xsNonEmpty fromXML

-- xsNonEmpty :: Parser a -> Parser (NonEmpty a)
-- xsNonEmpty parser = do
--     as <- xsSequence parser
--     case nonEmpty as of
--       Nothing -> addE "Expecting at least one match, found none"
--       Just ne -> return ne

-- Sequence (nestable)
-- Choice   (nestable)
--    <xs:choice minOccurs="0" maxOccurs="unbounded"> in mixed content... ugh
-- All

-- Mixed
