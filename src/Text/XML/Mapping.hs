{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Text.XML.Mapping
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- .
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- .

module Text.XML.Mapping where

import           Control.Applicative
import qualified Data.ByteString                      as S
import           Data.List.NonEmpty                   (NonEmpty)
import qualified Data.Text                            as T
import           Text.XML.Expat.Tree                  hiding (QName)
import           Text.XML.Mapping.Internal.Class
import qualified Text.XML.Mapping.Internal.ParseError as PE
import           Text.XML.Mapping.Internal.Parser
import           Text.XML.Mapping.Schema.Namespace
import           Text.XML.Mapping.Types

decode :: XML a => QName -> S.ByteString -> Either PE.ParseError a
decode qn bs = case parse' defaultParseOptions bs of
  Left xpe -> Left $ PE.reason (PE.malformed xpe) PE.at0
  Right n  -> runParser xml qn (Tag n)

data Baseline =
  Baseline { population :: Maybe T.Text
           , groups     :: NonEmpty Group
           , measures   :: NonEmpty Measure
           }
    deriving ( Eq, Ord )

instance XML Baseline where
  xml = Baseline
    <$> optional ("population" #> text)
    <*> "group_list" #> nonEmpty ("group" #> xml)
    <*> "measure_list" #> nonEmpty ("measure" #> xml)

data Group =
  Group { id         :: T.Text
        , groupTitle :: T.Text
        , groupDesc  :: Maybe T.Text
        }
    deriving ( Show, Eq, Ord )

instance XML Group where
  xml = Group
    <$> attr "group_id"
    <*> "title" #> text
    <*> optional ("description" #> text)

data Measure =
  Measure { measureTitle :: T.Text
          , measureDesc  :: Maybe T.Text
          , units        :: Maybe T.Text
          , param        :: Maybe T.Text
          , dispersion   :: Maybe T.Text
          , categories   :: NonEmpty MeasureCategory
          }
    deriving ( Show, Eq, Ord )

instance XML Measure where
  xml = Measure
    <$> "title" #> text
    <*> optional ("description" #> text)
    <*> optional ("units" #> text)
    <*> optional ("param" #> text)
    <*> optional ("dispersion" #> text)
    <*> "category_list" #> nonEmpty ("category" #> xml)

data MeasureCategory =
  MeasureCategory { subtitle     :: Maybe T.Text
                  , measurements :: NonEmpty Measurement
                  }
    deriving ( Show, Eq, Ord )

instance XML MeasureCategory where
  xml = MeasureCategory
    <$> optional ("subtitle" #> text)
    <*> "measurement_list" #> nonEmpty ("measurement" #> xml)

data Measurement =
  Measurement { groupId    :: T.Text
              , value      :: Maybe T.Text
              , spread     :: Maybe T.Text
              , lowerLimit :: Maybe T.Text
              , upperLimit :: Maybe T.Text
              , base       :: Maybe T.Text
              }
    deriving ( Show, Eq, Ord )

instance XML Measurement where
  xml = Measurement
    <$> attr "group_id"
    <*> optional (attr "value")
    <*> optional (attr "spread")
    <*> optional (attr "lower_limit")
    <*> optional (attr "upper_limit")
    <*> optional text






