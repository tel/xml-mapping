{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Text.XML.Expat.Mapping.Parser where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.ByteString (ByteString)
import Text.XML.Expat.Tree

data One
data Many
  
data ParseError = ParseError { trying :: First String, errors :: [String] }
                deriving ( Eq, Ord, Show )

instance Monoid ParseError where
  mempty = ParseError mempty mempty
  mappend (ParseError t1 e1) (ParseError t2 e2) = ParseError (t1 <> t2) (e1 <> e2)

anError :: String -> ParseError
anError e = ParseError mempty [e]

-- | The path through the XML tree
type Path = [NName ByteString]

newtype Parser (n :: *) a = P {
  unP :: [NNode ByteString]
         -> Path
         -> Either ParseError
                   (a, [NNode ByteString])
  } deriving ( Functor )

-- | Very secret function---interconvert between the Parser modalities
forgetP :: Parser n a -> Parser n' a
forgetP = P . unP

instance Applicative (Parser n) where
  pure a = P $ \ns _ -> Right (a, ns)

  -- This (<*>) instance is slightly better than `ap` in that it'll
  -- try the second branch even if the first fails and then combine
  -- the errors. This gives us slightly more comprehensive reporting.
  
  P pf <*> P px = P $ \ns p -> case pf ns p of
    Left e -> case px ns p of
      Left e' -> Left (e <> e')
      Right _   -> Left e -- We already failed above
    Right (f, ns') -> case px ns' p of
      Left e' -> Left e'
      Right (x, ns'') -> Right (f x, ns'')

instance Alternative (Parser n) where
  empty = P go where go _ _ = Left $ anError "Alternative {empty}"
  P p1 <|> P p2 = P $ \ns p -> case p1 ns p of
    r@Right{} -> r
    Left e -> case p2 ns p of
      r@Right{} -> r
      Left e' -> Left (e <> e')

instance Monad (Parser n) where
  return = pure
  P ma >>= f = P $ \ns p ->
    ma ns p >>= \(a, ns') ->
      unP (f a) ns' p
  fail s = P go where go _ _ = Left (anError s)
  
instance MonadPlus (Parser n) where
  mzero = empty
  mplus = (<|>)

-- | Add a setment to the 'Path'
underPath :: NName ByteString -> Parser n a -> Parser n a
underPath name (P p) = P $ \ns pth -> p ns (name:pth)

getNs :: Parser Many [NNode ByteString]
getNs = P go where go ns _ = Right (ns, ns)

getP :: Parser n Path
getP = P go where go ns p = Right (p, ns)

putNs :: [NNode ByteString] -> Parser Many ()
putNs ns = P go where go _ _ = Right ((), ns)

addE :: String -> Parser n a
addE e = P go where go _ _ = Left (anError e)

failPE :: ParseError -> Parser n a
failPE pe = P $ \_ _ -> Left pe

-- declare that we're "trying" something
declareT :: String -> Parser n a
declareT t = P go where go _ _ = Left (ParseError (First (Just t)) mempty)

infixl 4 <?>

-- How does this interact with the Path? How does it work at all? How
-- does Parsec use it?
-- 
-- I could implement everything atop a base error throwing function
-- like 'ParseError -> Parser n a' which decorates the 'ParseError'
-- with the 'Path'.

(<?>) :: Parser n a -> String -> Parser n a
p <?> e = p <* addE e

try1 :: (NNode ByteString -> Either ParseError a) -> Parser One a
try1 f = P go where go []     _ = Left (anError "Elements exhausted")
                    go (x:xs) _ = fmap (,xs) (f x)

parse1 :: Parser One a ->  NNode ByteString -> Path -> Either ParseError a
parse1 p e pth = fmap fst (unP p [e] pth)

parseM :: Parser Many a
         -> [NNode ByteString]
         -> Path
         -> Either ParseError (a, [NNode ByteString])
parseM = unP         
