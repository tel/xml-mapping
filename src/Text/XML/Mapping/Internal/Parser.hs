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
import           Text.XML.Mapping.Internal.Class
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

-- tryAtto :: Level -> [Tag] -> A.Parser a -> S.ByteString -> Either PE.ParseError (a, [Tag])
-- tryAtto ls ts atto bs = case A.parseOnly atto bs of
--   Left attoReason ->
--     Left $ PE.reasonAt (PE.simpleFail attoReason) ls
--   Right simple ->
--     Right (simple, ts)

-- instance X Parser where
--   pAttr atto qn = P $ \ls ts ->
--     case getAttr ls qn of
--       Nothing -> Left $ PE.reasonAt (PE.noAttr qn) ls
--       Just bs -> tryAtto ls ts atto bs

--   pText atto = P go where
--     go ls []     = Left $ PE.reasonAt PE.exhausted ls
--     go ls (t:ts) = case rawText t of
--       Nothing -> Left $ PE.reasonAt PE.expectingText ls
--       Just bs -> tryAtto ls ts atto bs

--   pElem qn pf = P go where
--     go ls []     = Left $ PE.reasonAt PE.exhausted ls
--     go ls (t:ts)
--       | ignorable t = go ls ts
--       | otherwise   = do

--         -- Build a new element context
--         ls' <- eitLevelError ls (step t ls)

--         -- Check that the element matches
--         unless (elemHere qn ls')
--           $ Left $ PE.reasonAt (PE.wrongElement qn) ls'

--         -- Run the inner parser on the element children
--         (res, leftovers) <- unP pf ls' (children t)

--         -- Ensure that all the children were consumed
--         unless (null leftovers)
--           $ Left $ PE.reasonAt PE.leftoverElements ls'

--         -- And we're done
--         return (El qn res, ts)

-- eitLevelError :: Level -> Either LevelError b -> Either PE.ParseError b
-- eitLevelError ls = either (\le -> Left $ PE.reasonAt (PE.levelError le) ls) Right

-- runParser :: Parser a -> Tag -> Either PE.ParseError a
-- runParser p t = do
--   (res, leftovers) <- unP p level0 [t]
--   unless (null leftovers) $ Left $ PE.reasonAt PE.leftoverElements level0
--   return res
