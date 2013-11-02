{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

-- |
-- Module      : Text.XML.Mapping.Internal.Parser
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- .
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- .
-- Low-level definition of the parser.
-- .

module Text.XML.Mapping.Internal.Parser where

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec                      as A
import qualified Data.ByteString                      as S
import qualified Data.HashMap.Strict                  as Map
import           Data.Semigroup
import           Text.XML.Mapping.Internal.Class
import           Text.XML.Mapping.Internal.LevelSet
import qualified Text.XML.Mapping.Internal.ParseError as PE
import           Text.XML.Mapping.Schema.Namespace
import           Text.XML.Mapping.Types

newtype Parser a = P {

  unP :: LevelSet -> [Tag] -> Either PE.ParseError (a, [Tag])

  } deriving Functor

instance Applicative Parser where
  pure a = P $ \_ls ts -> Right (a, ts)

  P pf <*> P px = P $ \ls ts -> case pf ls ts of
    Left pef -> case px ls ts of
      Left pex -> Left (pef `PE.seq` PE.speculated pex) -- combine the errors
      Right _  -> Left pef
    Right (f, ts') -> case px ls ts' of
      Left pex -> Left pex
      Right (x, ts'') -> Right (f x, ts'')

instance Alternative Parser where
  empty = P $ \ls _ts -> Left $ PE.reasonAt PE.empty ls

  P p1 <|> P p2 = P $ \ls ts -> case p1 ls ts of
    Left pe1 -> case p2 ls ts of
      Left pe2 -> Left (pe1 `PE.or` pe2)
      Right x  -> Right x
    Right x -> Right x

instance Semigroup (Parser a) where
  (<>) = (<|>)

instance Monoid (Parser a) where
  mempty = empty
  mappend = (<|>)

tryAtto :: LevelSet -> [Tag] -> A.Parser a -> S.ByteString -> Either PE.ParseError (a, [Tag])
tryAtto ls ts atto bs = case A.parseOnly atto bs of
  Left attoReason ->
    Left $ PE.reasonAt (PE.simpleFail attoReason) ls
  Right simple ->
    Right (simple, ts)

instance X Parser where
  pAttr atto qn = P $ \ls ts ->
    case Map.lookup qn (attributes . levelState $ ls) of
      Nothing -> Left $ PE.reasonAt (PE.noAttr qn) ls
      Just bs -> tryAtto ls ts atto bs

  pText atto = P go where
    go ls []     = Left $ PE.reasonAt PE.exhausted ls
    go ls (t:ts) = case rawText t of
      Nothing -> Left $ PE.reasonAt PE.expectingText ls
      Just bs -> tryAtto ls ts atto bs

  pElem qn pf = P go where
    go ls []     = Left $ PE.reasonAt PE.exhausted ls
    go ls (t:ts)
      | ignorable t = go ls ts
      | otherwise   = do

        -- Build a new element context
        ls' <- eitLevelError ls $ t !<< ls

        -- Check that the element matches
        unless (elemHere qn ls')
          $ Left $ PE.reasonAt (PE.wrongElement qn) ls'

        -- Run the inner parser on the element children
        (res, leftovers) <- unP pf ls' (children t)

        -- Ensure that all the children were consumed
        unless (null leftovers)
          $ Left $ PE.reasonAt PE.leftoverElements ls'

        -- And we're done
        return (res, ts)

eitLevelError :: LevelSet -> Either LevelError b -> Either PE.ParseError b
eitLevelError ls = either (\le -> Left $ PE.reasonAt (PE.levelError le) ls) Right

runParser :: Parser a -> QName -> Tag -> Either PE.ParseError a
runParser p qn t = do
  lstate <- case initialize t of
    Left le -> Left $ PE.reason (PE.levelError le) PE.at0
    Right x -> Right x
  let ls = Root lstate
  (res, leftovers) <- unP (pElem qn p) ls [t]
  unless (null leftovers) $ Left $ PE.reasonAt PE.leftoverElements ls
  return res
