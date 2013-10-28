
module Main where

import           Test.Tasty
import qualified Test.Text.XML.Expat.Mapping as TXEMapping

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "hexpat-mapping"
        [ TXEMapping.tests
        ]
