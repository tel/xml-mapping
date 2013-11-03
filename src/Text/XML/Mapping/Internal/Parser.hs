{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

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
import qualified Data.Attoparsec                      as A
import qualified Data.ByteString                      as S
import           Data.Semigroup
import           Text.XML.Mapping.Internal.Class      (El (El), X (..))
import           Text.XML.Mapping.Internal.Err
import           Text.XML.Mapping.Internal.Level
import           Text.XML.Mapping.Internal.ParseError
import           Text.XML.Mapping.Types

newtype Parser a = P {
  unP :: Level -> [Tag] -> (Err ParseError a, [Tag])
  } deriving Functor

instance Applicative Parser where
  pure a = P $ \l ts -> (ate l $ pure a, ts)

  P pf <*> P px = P $ \l ts ->
    let
      (errf, ts' ) = pf l ts
      (errx, ts'') = px l ts'
    in
     (ate l $ perE errf <*> perE errx, ts'')

instance Alternative Parser where
  empty = P $ \l ts -> (ate l empty, ts)

  P p1 <|> P p2 = P $ \l ts ->
    let
      (err1, ts1) = p1 l ts
      (err2, ts2) = p2 l ts
    in
     uncarry ts . ate l $ carry ts1 err1 <|> carry ts2 err2

    where

      -- The carry/uncarry functions store the remaining tags of each
      -- branch of (<|>) along with the result and then unfold it at
      -- the end (along with a "don't consume anything" default if
      -- needed).

      carry :: [Tag] -> Err ParseError a -> Err ParseErrorR (a, [Tag])
      carry ts = perE . fmap (,ts)

      uncarry :: [Tag] -> Err ParseError (a, [Tag]) -> (Err ParseError a, [Tag])
      uncarry ts (Err e)      = (Err e, ts)
      uncarry _  (Ok (a, ts)) = (Ok  a, ts)

instance Semigroup (Parser a) where
  (<>) = (<|>)

instance Monoid (Parser a) where
  mempty = empty
  mappend = (<|>)

tryAtto :: Level -> A.Parser a -> S.ByteString -> Err ParseError a
tryAtto l atto bs = case A.parseOnly atto bs of
  Left  attoReason -> Err $ simpleFail attoReason +++ l
  Right simple     -> Ok simple

withOne :: (Level -> Tag -> Err ParseError a) -> Level -> [Tag] -> (Err ParseError a, [Tag])
withOne _  l []     = (Err $ exhausted +++ l, [])
withOne go l (t:ts)
  | ignorable t = withOne go l ts
  | otherwise   =
    case go l t of
      Err pe -> (Err pe, t:ts)
      Ok  a  -> (Ok  a ,   ts)

instance X Parser where
  pAttr atto qn = P $ \l ts -> (go l, ts) where
    go l = case getAttr l qn of
      Nothing -> Err $ noAttr qn +++ l
      Just bs -> tryAtto l atto bs

  pText atto = P (withOne go) where
    go l t = case rawText t of
      Nothing -> Err (expectingText +++ l)
      Just bs -> tryAtto l atto bs

  pElem checkQN pf = P (withOne go) where
    go l t =
      -- Build a new element context
      case step t l of
        Left levelE -> Err (levelError levelE +++ l)
        Right l' ->
          let Just qn = name l'
          -- Check that the element matches
          in if not (checkQN qn)
             then Err (wrongElement +++ l')
             -- Run the inner parser on the element children
             else case unP pf l' (children t) of
               (Err pe, _) -> Err pe
               (Ok  a , leftovers)
                 -- Fail if there are leftovers
                 | not (null leftovers) -> Err (leftoverElements +++ l')
                 | otherwise            -> Ok (El qn a)

errLevelError :: Level -> Either LevelError a -> Err ParseError a
errLevelError l = either (\le -> Err $ levelError le +++ l) Ok

runParser :: Parser a -> Tag -> Either ParseError a
runParser p t =
  let (res, _leftovers) = unP p level0 [t]
  in errEither res
