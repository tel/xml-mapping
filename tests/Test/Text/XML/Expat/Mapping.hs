
module Test.Text.XML.Expat.Mapping (
  main, tests
  ) where

import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Text.XML.Expat.Mapping" []

