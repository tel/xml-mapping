{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Text.XML.Mapping.Internal.Err
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- .
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- .
-- A version of 'Either' with more 'Applicative'-friendly instances.
-- .

module Text.XML.Mapping.Internal.Err where

import           Control.Applicative
import           Text.XML.Mapping.Internal.NearSemiring

-- | Nothing more than 'Either' with interesting instances for
-- 'Applicative' and 'Alternative'.
data Err e a = Err e | Ok a deriving Functor

-- 'Err' eliminator
err :: (e -> c) -> (a -> c) -> Err e a -> c
err f _ (Err e) = f e
err _ s (Ok  a) = s a

errEither :: Err e a -> Either e a
errEither = err Left Right

instance NearSemiring e => Applicative (Err e) where
  pure = Ok
  Err e <*> Err e' = Err (e ~*~ e')
  Err e <*> _      = Err e
  _     <*> Err e  = Err e
  Ok  f <*> Ok  x  = Ok (f x)

instance NearSemiring e => Alternative (Err e) where
  empty = Err zero
  Err e <|> Err e' = Err (e ~+~ e')
  Err _ <|> go     = go
  go    <|> _      = go
