{-# LANGUAGE DeriveGeneric #-}

module Expression ( Expression(E,L)
                  , PrettyPrint
                  , pretty
                  , SExpression
                  , SLeaf, makeAtom, makeNumber ) where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import Data.Traversable (Traversable, fmapDefault, foldMapDefault)

import Data.Char

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneof)

data Expression a = E a
                  | L [Expression a]
                  deriving (Show)

instance (Arbitrary a) => Arbitrary (Expression a) where
  arbitrary = oneof [E <$> arbitrary, L <$> arbitrary]

instance (Eq a) => Eq (Expression a) where
  E a  == E b  = a == b
  L as == L bs = as == bs
  _    == _    = False

instance Functor Expression where
  fmap = fmapDefault

instance Foldable Expression where
  foldMap = foldMapDefault

instance Traversable Expression where
  traverse f (E x)  = E <$> f x
  traverse f (L es) = L <$> traverse (traverse f) es 

instance Applicative Expression where
  pure x = E x
  E f <*> e  = fmap f e
  L fs <*> e = L $ map (<*> e) fs

instance Monad Expression where
  return x = E x
  E e >>= f  = f e
  L es >>= f = L $ map (>>= f) es

data SLeaf = A String
           | N Integer
           deriving (Eq, Show, Generic)

instance Hashable SLeaf

instance Arbitrary SLeaf where
  arbitrary = oneof [A <$> arbitrary, N <$> arbitrary]

type SExpression = Expression SLeaf

isIdentifier :: String -> Bool
isIdentifier [] = False
isIdentifier (x:[]) = isAlpha x
isIdentifier (x:xs) = isAlpha x && and (map isAlphaNum xs)

makeAtom :: String -> SLeaf
makeAtom a | isIdentifier a = A a
           | otherwise = error "invalid identifier"

makeNumber :: Integer -> SLeaf
makeNumber n = N n

class PrettyPrint a where
  pretty :: a -> String

instance (PrettyPrint a) => PrettyPrint (Expression a) where
  pretty (E e)  = pretty e
  pretty (L es) = "(" ++ unwords (fmap pretty es) ++ ")"

instance PrettyPrint SLeaf where
  pretty (A s) = s
  pretty (N i) = show i
