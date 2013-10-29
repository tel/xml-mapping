{-# LANGUAGE OverloadedStrings #-}

module Tests.Text.XML.Expat.Mapping (
  main, tests
  ) where

import           Control.Error
import           Data.ByteString     (ByteString)
import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.XML.Expat.Tree

main :: IO ()
main = defaultMain tests


treeOf :: ByteString -> NNode ByteString
treeOf = toNamespaced . toQualified . fromJust . hush . parse' defaultParseOptions


tests :: TestTree
tests =
  testGroup "Text.XML.Expat.Mapping"
  [ testCase "Basic test 1" (eChildren (treeOf "<foo />") @?= [])
  , testCase "Basic test 2" (eChildren (treeOf "<foo>Hi</foo>") @?= [Text "hi"])
  ]

