{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.XML.Expat.Mapping where

import Control.Error
import Text.XML.Expat.Tree
import Text.XML.Expat.Mapping.Parser
import Data.ByteString (ByteString)

basicParse :: ByteString -> NNode ByteString
basicParse = toNamespaced . toQualified . fromJust . hush . parse' defaultParseOptions
  where fromJust (Just x) = x

ex1 :: NNode ByteString
ex1 = basicParse "<foo> bar <baz /> quux </foo>"

class FromXML n a | a -> n where
  fromXML :: Parser n a

node :: NName ByteString -> Parser Many a -> Parser One a
node nname parser = underPath nname (try1 go) where
  go Text{} = Left (anError "Found text when expecting an element")
  go e      = fmap fst (parseM parser (eChildren e) [])

node' :: ByteString -> Parser Many a -> Parser One a
node' n = node (NName Nothing n)

-- | Represents data 'Mixed' with text.
data Mixed a = Mixed { unMixed :: [Either ByteString a] }
             deriving ( Eq, Show, Ord )

instance FromXML One a => FromXML Many (Mixed a) where
  fromXML = getNs >>= fmap Mixed . mapM go where
    go (Text t)    = return (Left t)
    go e@Element{} = do
      p <- getP
      case parse1 fromXML e p of
        Left pe -> failPE pe
        Right a -> return (Right a)

instance FromXML One (NNode ByteString) where
  fromXML = try1 Right

-- Sequence (nestable)
-- Choice   (nestable)
-- All

-- Mixed

-- | Defaults out a 'Maybe' parser
def :: a -> Parser n (Maybe a) -> Parser n a
def val p = do
  mayA <- p
  case mayA of
    Nothing -> return val
    Just a  -> return a

-- | Matches only when the 'Eq'ual to a sentinel value.
fixed :: (Show a, Eq a) => a -> Parser n a -> Parser n a
fixed val p = do
  a <- p
  if a == val
    then return a
    else fail ("Did not match fixed value: " ++ show val)

fixed' :: (Eq a) => a -> Parser n a -> Parser n a
fixed' val p = do
  a <- p
  if a == val
    then return a
    else fail "Did not some fixed value"
