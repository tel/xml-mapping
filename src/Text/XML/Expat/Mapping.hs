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

import           Control.Applicative
import           Control.Error
import           Data.ByteString                        (ByteString)
import           Data.Foldable                          (foldMap)
import           Data.List.NonEmpty                     (NonEmpty (..),
                                                         nonEmpty)
import           Data.Text                              (Text)
import           Data.Text.Encoding                     (decodeUtf8')
import           Text.XML.Expat.Mapping.Internal.Parser
import           Text.XML.Expat.Tree

class FromXML a where
  fromXML :: Parser a

-- | Consumes any number of Text nodes and concatenates them with
-- spaces.
instance FromXML Text where
  fromXML = do
    es <- getNs
    justZ (foldMap go es) <|> fail "Saw an Element; expecting only Text."
    where
      go Element{} = Nothing
      go (Text t)  = hush (decodeUtf8' t)

node :: NName ByteString -> Parser a -> Parser a
node nname parser = try1 go where
  go Text{} = Left (anError "Found text when expecting an element")
  go e      =
    if eName e == nname
    then fst <$> parseM parser (eChildren e)
    else Left (anError $ "Expecting element named: " ++ show nname ++ "; saw " ++ show (eName e))

node' :: ByteString -> Parser a -> Parser a
node' n = node (NName Nothing n)

getA :: Parser (Maybe (Attributes (NName ByteString) ByteString))
getA = withNs go where
  go []            = (Nothing, [])
  go es@(Text{}:_) = (Nothing, es)
  go es@(e     :_) = (Just (eAttributes e), es)

-- | Represents data 'Mixed' with text.
data Mixed a = Mixed { unMixed :: [Either ByteString a] }
             deriving ( Eq, Show, Ord )

instance FromXML a => FromXML (Mixed a) where
  fromXML = getNs >>= fmap Mixed . mapM go where
    go (Text t)    = return (Left t)
    go e@Element{} = case parse1 fromXML e of
      Left pe -> failPE pe
      Right a -> return (Right a)

instance FromXML a => FromXML [a] where
  fromXML = xsSequence fromXML

xsSequence :: Parser a -> Parser [a]
xsSequence parser = withNs tryEm
  where
    tryEm = tryEm' []
    tryEm' as []     = (reverse as, [])
    tryEm' as (n:ns) = case parse1 parser n of
      Left _  -> (reverse as, n:ns)
      Right a -> tryEm' (a:as) ns

instance FromXML a => FromXML (NonEmpty a) where
  fromXML = xsNonEmpty fromXML

xsNonEmpty :: Parser a -> Parser (NonEmpty a)
xsNonEmpty parser = do
    as <- xsSequence parser
    case nonEmpty as of
      Nothing -> addE "Expecting at least one match, found none"
      Just ne -> return ne

instance FromXML a => FromXML (Maybe a) where
  fromXML = maybe1 fromXML

instance FromXML (NNode ByteString) where
  fromXML = try1 Right

-- Sequence (nestable)
-- Choice   (nestable)
--    <xs:choice minOccurs="0" maxOccurs="unbounded"> in mixed content... ugh
-- All

-- Mixed

-- | Defaults out a 'Maybe' parser
def :: a -> Parser (Maybe a) -> Parser a
def val p = do
  mayA <- p
  case mayA of
    Nothing -> return val
    Just a  -> return a

-- | Matches only when the 'Eq'ual to a sentinel value.
fixed :: (Show a, Eq a) => a -> Parser a -> Parser a
fixed val p = do
  a <- p
  if a == val
    then return a
    else fail ("Did not match fixed value: " ++ show val)

fixed' :: (Eq a) => a -> Parser a -> Parser a
fixed' val p = do
  a <- p
  if a == val
    then return a
    else fail "Did not some fixed value"
