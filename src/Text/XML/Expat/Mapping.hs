{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Text.XML.Expat.Mapping where

import Control.Applicative
import Control.Monad
import Control.Error
import Data.Monoid
import Data.ByteString (ByteString)
import Text.XML.Expat.Tree

data One
data Many
  
data ParseError = ParseError { trying :: First String, errors :: [String] }

instance Monoid ParseError where
  mempty = ParseError mempty mempty
  mappend (ParseError t1 e1) (ParseError t2 e2) = ParseError (t1 <> t2) (e1 <> e2)

anError :: String -> ParseError
anError e = ParseError mempty [e]

-- | The path through the XML tree
type Path = [ByteString]

newtype Parser (n :: *) a = P {
  unP :: [UNode ByteString] -> Path -> Either ParseError (a, [UNode ByteString])
  } deriving ( Functor )

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

instance Monad (Parser n) where
  return = pure
  P ma >>= f = P $ \ns p -> do
    ma ns p >>= \(a, ns') -> do
      unP (f a) ns' p

getNs :: Parser Many [UNode ByteString]
getNs = P go where go ns _ = Right (ns, ns)

putNs :: [UNode ByteString] -> Parser Many ()
putNs ns = P go where go _ _ = Right ((), ns)

addE :: String -> Parser n a
addE e = P go where go _ _ = Left (anError e)

setT :: String -> Parser n a
setT t = P go where go _ _ = Left (ParseError (First (Just t)) mempty)

infixl 4 <?>

(<?>) :: Parser n a -> String -> Parser n a
p <?> e = p <* addE e

try1 :: (UNode ByteString -> Either ParseError a) -> Parser One a
try1 f = P go where go []     _ = Left (anError "Elements exhausted")
                    go (x:xs) _ = fmap (,xs) (f x)

