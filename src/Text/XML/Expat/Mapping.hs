{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Control.Monad.State
import           Data.List.NonEmpty                         (NonEmpty)
import qualified Data.List.NonEmpty                         as NEL
import           Data.Text                                  (Text)
import           GHC.Generics                               (Generic)

import           Text.XML.Expat.Mapping.Internal.Namespaces
import           Text.XML.Expat.Mapping.Internal.Parser

data Baseline =
  Baseline { population :: Maybe Text
           , groups     :: NonEmpty Group
           , measures   :: NonEmpty Measure
           }
    deriving ( Eq, Ord, Generic )

xsOpt :: Parser a -> Parser (Maybe a)
xsOpt = xsOptional

xsMany :: Parser a -> Parser [a]
xsMany parser = P $ do
  es <- get
  go [] es
  where
    go ret []      = put [] >> return (reverse ret)
    go ret (e:es') = case runParser parser e of
      Left _  -> put (e:es') >> return (reverse ret)
      Right a -> go (a:ret) es'

xsSome :: Parser a -> Parser (NonEmpty a)
xsSome par =  P $ do
  res <- parMany
  case res of
    []     -> fail "Expecting at least one element"
    (x:xs) -> return (x NEL.:| xs)
  where
    (P parMany) = xsMany par

(#>) :: NamespaceName -> Parser a -> Parser a
(#>) = xsElement

instance FromXML Baseline where -- multiple elements in sequence
  fromXML = Baseline <$> xsOpt ("population" #> xsSimple)
                     <*> "group_list" #> xsSome ("group" #> fromXML)
                     <*> "measure_list" #> xsSome ("measure" #> fromXML)

data Group =
  Group { id         :: Text
        , groupTitle :: Text
        , groupDesc  :: Maybe Text
        }
    deriving ( Show, Eq, Ord, Generic )

instance FromXML Group where
  fromXML = Group <$> attr "group_id"
                  <*> "title" #> xsSimple
                  <*> xsOpt ("description" #> xsSimple)

data Measure =
  Measure { measureTitle :: Text
          , measureDesc  :: Maybe Text
          , units        :: Maybe Text
          , param        :: Maybe Text
          , dispersion   :: Maybe Text
          , categories   :: NonEmpty MeasureCategory
          }
    deriving ( Show, Eq, Ord, Generic )

instance FromXML Measure where
  fromXML = Measure <$> "title" #> xsSimple
                    <*> xsOpt ("description" #> xsSimple)
                    <*> xsOpt ("units" #> xsSimple)
                    <*> xsOpt ("param" #> xsSimple)
                    <*> xsOpt ("dispersion" #> xsSimple)
                    <*> "category_list" #> xsSome ("category" #> fromXML)

data MeasureCategory =
  MeasureCategory { subtitle     :: Maybe Text
                  , measurements :: NonEmpty Measurement
                  }
    deriving ( Show, Eq, Ord, Generic )

instance FromXML MeasureCategory where
  fromXML = MeasureCategory
            <$> xsOpt ("subtitle" #> xsSimple)
            <*> "measurement_list" #> xsSome ("measurement" #> fromXML)

data Measurement =
  Measurement { groupId    :: Text
              , value      :: Maybe Text
              , spread     :: Maybe Text
              , lowerLimit :: Maybe Text
              , upperLimit :: Maybe Text
              , base       :: Maybe Text
              }
    deriving ( Show, Eq, Ord, Generic )

instance FromXML Measurement where
  fromXML = Measurement
            <$> attr "group_id"
            <*> xsOpt (attr "value")
            <*> xsOpt (attr "spread")
            <*> xsOpt (attr "lower_limit")
            <*> xsOpt (attr "upper_limit")
            <*> xsOpt xsSimple

-- What are the "ways" of parsing a single element?
-- Given a NName, we might Maybe, Either, [], NonEmpty

-- What are the "ways" of parsing a list of elements?
