-- |
-- Module      : Text.XML.Mapping.Internal.ParseError
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- .
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable

module Text.XML.Mapping.Internal.ParseError where

import           Control.Applicative
import qualified Text.XML.Expat.Tree                    as Ex
import           Text.XML.Mapping.Internal.Err
import           Text.XML.Mapping.Internal.Level
import           Text.XML.Mapping.Internal.NearSemiring
import           Text.XML.Mapping.Schema.Namespace

-- | 'ParseError' \"in a 'Control.Monad.Reader.Reader'\". ParseError
-- itself cannot be a 'NearSemiring' because it needs a 'Level' in
-- order to construct a 'zero', but 'ParseErrorR' can be by delaying
-- the need for that 'Level'.
newtype ParseErrorR = PER { at :: Level -> ParseError }

per :: ParseError -> ParseErrorR
per = PER . const

perE :: Err ParseError a -> Err ParseErrorR a
perE = err (Err . per) Ok

-- Feed 'Level' information into an 'Err' with a 'ParseErrorR'.
ate :: Level -> Err ParseErrorR a -> Err ParseError a
ate l = err (Err . flip at l) Ok

instance NearSemiring ParseErrorR where
  zero            = PER $ At (Reason Empty)
  PER a ~*~ PER b = PER $ Seq <$> a <*> b
  PER a ~+~ PER b = PER $ Alt <$> a <*> b

-- | Abstract for now.
data ParseError = Seq ParseError ParseError
                | Alt ParseError ParseError
                | At  ParseError Level
                | Reason Reason
                deriving ( Show, Eq )

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

-- | ParseError at a particular location
(+++) :: ParseError -> Level -> ParseError
(+++) = At

-- | Append a new reason for failure
reason :: Reason -> ParseError
reason = Reason

-- | Parse error due to inability to even parse the initial string
malformed :: Ex.XMLParseError -> ParseError
malformed = reason . MalformedXML

-- | Could not find attribute
noAttr :: QName -> ParseError
noAttr = reason . NoAttr

-- | Could not parse a simple type
simpleFail :: String -> ParseError
simpleFail = reason . SimpleFailure

-- | No more tags
exhausted :: ParseError
exhausted = reason Exhausted

-- | Expecting Text
expectingText :: ParseError
expectingText = reason ExpectingText

levelError :: LevelError -> ParseError
levelError = reason . LevelError

wrongElement :: QName -> ParseError
wrongElement = reason . WrongElement

leftoverElements :: ParseError
leftoverElements = reason LeftoverElements
