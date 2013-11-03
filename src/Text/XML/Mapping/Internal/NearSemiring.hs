-- |
-- Module      : Text.XML.Mapping.Internal.NearSemiring
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- .
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- .
-- A (left) 'NearSemiring' class. Nothing special.
-- .

module Text.XML.Mapping.Internal.NearSemiring where

infixl 7 ~*~
infixl 6 ~+~
-- | A class where
-- .
-- * 'zero' & '(~+~)' are a monoid
-- .
-- ** @ forall $ \a     -> a ~+~ zero == zero ~+~ a == a             @
-- ** @ forall $ \a b c -> a ~+~ (b ~+~ c) == (a ~+~ b) ~+~ c        @
-- .
-- * 'mult' is a semigroup product
-- .
-- ** @ forall $ \a b c -> a ~*~ (b ~*~ c) == (a ~*~ b) ~*~ c        @
-- .
-- * 'zero' is a left zero of '(~*~)'
-- .
-- ** @ forall $ \a     -> zero ~*~ a == zero                        @
-- .
-- * `(~+~)` left distributes over '(~*~)'
-- .
-- ** @ forall $ \a b c -> (a ~+~ b) ~*~ c = (a ~*~ c) ~+~ (b ~*~ c) @
-- .
class NearSemiring a where
  zero  :: a
  (~+~) :: a -> a -> a
  (~*~) :: a -> a -> a
