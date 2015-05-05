{-# LANGUAGE DeriveGeneric #-}

module Expression.Pattern (Pattern, PLeaf(P,S)) where

import Expression (Expression, PrettyPrint, pretty)

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneof)

data PLeaf a = S a
             | P a
             deriving (Show, Eq, Generic)

instance (Hashable a) => Hashable (PLeaf a)

instance (Arbitrary a) => Arbitrary (PLeaf a) where
  arbitrary = oneof [S <$> arbitrary, P <$> arbitrary]

type Pattern a = Expression (PLeaf a)

instance (PrettyPrint a) => PrettyPrint (PLeaf a) where
  pretty (S e) = pretty e
  pretty (P s) = (pretty s) ++ "_"
