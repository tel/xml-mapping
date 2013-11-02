-- |
-- Module      : Text.XML.Mapping.Internal.ParseError
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- .
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable

module Text.XML.Mapping.Internal.ParseError where

import           Prelude                            hiding (or, seq)
import qualified Text.XML.Expat.Tree                as Ex
import           Text.XML.Mapping.Internal.LevelSet
import           Text.XML.Mapping.Schema.Namespace

-- | Abstract for now.
data ParseError

parseError :: ParseError
parseError = error "ParseError {DNE!}"

data Reason = NoAttr QName
            | Empty
            | MalformedXML Ex.XMLParseError
            | SimpleFailure String
            | Exhausted
            | ExpectingText
            | LevelError LevelError
            | WrongElement QName
            | LeftoverElements

-- | Primary constructor
at :: LevelSet -> ParseError
at _ = parseError

at0 :: ParseError
at0 = parseError

-- | Append a new reason for failure
reason :: Reason -> ParseError -> ParseError
reason _ _ = parseError

reasonAt :: Reason -> LevelSet -> ParseError
reasonAt r ls = reason r (at ls)

seq :: ParseError -> ParseError -> ParseError
seq pe1 _pe2 = pe1

or :: ParseError -> ParseError -> ParseError
or pe1 _pe2 = pe1

empty :: Reason
empty = Empty

-- | Parse error due to inability to even parse the initial string
malformed :: Ex.XMLParseError -> Reason
malformed = MalformedXML

-- | Could not find attribute
noAttr :: QName -> Reason
noAttr = NoAttr

-- | Could not parse a simple type
simpleFail :: String -> Reason
simpleFail = SimpleFailure

-- | No more tags
exhausted :: Reason
exhausted = Exhausted

-- | Expecting Text
expectingText :: Reason
expectingText = ExpectingText

levelError :: LevelError -> Reason
levelError = LevelError

wrongElement :: QName -> Reason
wrongElement = WrongElement

leftoverElements :: Reason
leftoverElements = LeftoverElements

-- | Denotes a parse error that only occurred speculatively.
speculated :: ParseError -> ParseError
speculated = id
