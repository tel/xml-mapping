-- |
-- Module      : Text.XML.Mapping.Internal.ParseError
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- .
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable

module Text.XML.Mapping.Internal.ParseError where

import           Prelude                           hiding (or, seq)
import qualified Text.XML.Expat.Tree               as Ex
import           Text.XML.Mapping.Internal.Level
import           Text.XML.Mapping.Schema.Namespace

-- | Abstract for now.
data ParseError = Seq ParseError ParseError
                | Or  ParseError ParseError
                | Speculated ParseError
                | ParseError { peLoc    :: Maybe Level
                             , peReason :: [Reason]
                             }
                deriving ( Eq )

data Reason = NoAttr QName
            | Empty
            | MalformedXML Ex.XMLParseError
            | SimpleFailure String
            | Exhausted
            | ExpectingText
            | LevelError LevelError
            | WrongElement QName
            | LeftoverElements
            deriving ( Show, Eq )

-- | Primary constructor
at :: Level -> ParseError
at ls = ParseError (Just ls) []

at0 :: ParseError
at0 = ParseError Nothing []

-- | Append a new reason for failure
reason :: Reason -> ParseError -> ParseError
reason r (ParseError loc res) = ParseError loc (r:res)
reason r (Seq pe1 pe2) = Seq (reason r pe1) (reason r pe2)
reason r (Or  pe1 pe2) = Or  (reason r pe1) (reason r pe2)
reason r (Speculated pe) = Speculated (reason r pe)

reasonAt :: Reason -> Level -> ParseError
reasonAt r ls = reason r (at ls)

seq :: ParseError -> ParseError -> ParseError
seq = Seq

or :: ParseError -> ParseError -> ParseError
or = Or

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
speculated = Speculated
