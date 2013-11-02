{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Text.XML.Expat.Mapping.Internal.Namespaces where

import           Control.Lens
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Char8             as S8
import           Data.Hashable
import qualified Data.HashMap.Strict               as Map
import           Data.List
import           Data.Monoid
import           Data.String
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           GHC.Generics                      (Generic)

import           Text.XML.Mapping.Schema.Namespace

makeLenses ''NSMap
