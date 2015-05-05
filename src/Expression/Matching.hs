module Expression.Matching ( Rule(Rule)
                           , meta
                           , match
                           , bind
                           , rewriteWith
                           , rewriteAllWith) where

import Expression (Expression(E,L))
import Expression.Pattern (Pattern, PLeaf(P,S))

import Prelude hiding (foldr, sequence)

import Data.Foldable (fold)

import Data.Traversable (sequence)

import qualified Data.HashMap.Strict as M

import Data.Hashable (Hashable)

meta :: Pattern a -> Pattern (PLeaf a)
meta = fmap f
  where
    f (P p) = P (S p)
    f (S e) = S (S e)

match :: (Eq a, Hashable a) => (Pattern a) -> Expression a -> Maybe (M.HashMap a (Expression a))
match p = fmap fold . sequence . f p
  where
    f (E (P s)) e                               = E (Just (M.singleton s e))
    f (E (S p)) (E e)  | p == e                 = E (Just M.empty)
    f (L as)    (L bs) | length as == length bs = L (zipWith f as bs)
    f _         _                               = E Nothing

bind :: (Eq a, Hashable a) => M.HashMap a (Expression a) -> Expression a -> Expression a
bind bs (E e) | M.member e bs = bs M.! e
              | otherwise     = E e
bind bs (L es) = L (fmap (bind bs) es)

data Rule a = Rule (Pattern a) (Expression a)

rewriteWith :: (Eq a, Hashable a) => Rule a -> Expression a -> Expression a
rewriteWith (Rule p r) e =
  case match p e of
    Nothing -> e
    Just bs -> bind bs r

rewriteAllWith :: (Eq a, Hashable a) => Rule a -> Expression a -> Expression a
rewriteAllWith (Rule p r) e =
  case match p e of
    Nothing ->
      case e of
        E a -> e
        L es -> L $ fmap (rewriteAllWith (Rule p r)) es
    Just bs -> bind bs r
