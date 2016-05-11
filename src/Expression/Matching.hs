module Expression.Matching ( Rule(Rule)
                           , meta
                           , match
                           , bind
                           , mergeHashMap
                           , rewriteWith
                           , rewriteAllWith) where

import Expression (Expression(E,L))
import Expression.Pattern (Pattern, PLeaf(P,PS,PNS,S))

import Prelude hiding (foldr, sequence)

import Data.List
import Data.Maybe (maybeToList)

import qualified Data.HashMap.Strict as M

import Data.Hashable (Hashable)

meta :: Pattern a -> Pattern (PLeaf a)
meta = fmap f
  where
    f (P p)   = P (S p)
    f (PS p)  = PS (S p)
    f (PNS p) = PNS (S p)
    f (S e)   = S (S e)

mergeHashMap :: (Eq k, Hashable k, Eq v) => M.HashMap k v -> M.HashMap k v -> Maybe (M.HashMap k v)
mergeHashMap ma = M.foldlWithKey' f (Just ma)
  where f (Just acc) k v = if M.member k acc
                             then if (acc M.! k) == v
                               then Just acc
                               else Nothing
                             else Just (M.insert k v acc)
        f Nothing _ _ = Nothing

match :: (Eq a, Hashable a) => (Pattern a) -> Expression a -> [M.HashMap a [Expression a]]
match (E (P a)) e = [M.singleton a [e]]
match (E (PS a)) e = [M.singleton a [e]]
match (E (PNS a)) e = [M.singleton a [e]]
match (E (S p)) (E e) | p == e = [M.empty]
match (L ps) (L es) = match' ps es
  where
    match' [] [] = [M.empty]
    match' (E (S p):ps') (E e:es') | p == e = match' ps' es'
    match' (E (P p):ps') (e:es') = mappend (M.singleton p [e]) <$> match' ps' es'
    match' (E (PS p):ps') es' = f =<< tail (zip (inits es') (tails es'))
      where f (a, b) = concat (maybeToList . mergeHashMap (M.singleton p a) <$> match' ps' b)
    match' (E (PNS p):ps') es' = f =<< (zip (inits es') (tails es'))
      where f (a, b) = concat (maybeToList . mergeHashMap (M.singleton p a) <$> match' ps' b)
    match' (L p:ps') (L e:es') = merge (match' p e) (match' ps' es')
    match' _ _ = []

    merge xs ys = concat $ maybeToList <$> ((\x -> mergeHashMap x <$> ys) =<< xs)

match (E (S _)) _ = []
match (L _) (E _) = []


bind :: (Eq a, Hashable a) => M.HashMap a [Expression a] -> Expression a -> Expression a
bind bs (E e) | M.member e bs = let d = bs M.! e in if length d == 1
                                                        then d !! 0
                                                        else L d
              | otherwise     =  E e
bind bs (L es) = L (bind' es)
  where
    bind' (E e:es') | M.member e bs = bs M.! e ++ bind' es'
    bind' (L e:es')                 = L (bind' e) : bind' es'
    bind' (e:es')                   = e : bind' es'
    bind' []                        = []

data Rule a = Rule (Pattern a) (Expression a)

rewriteWith :: (Eq a, Hashable a) => Rule a -> Expression a -> Expression a
rewriteWith (Rule p r) e =
  case match p e of
    []      -> e
    bs      -> bind (head bs) r

rewriteAllWith :: (Eq a, Hashable a) => Rule a -> Expression a -> Expression a
rewriteAllWith (Rule p r) e =
  case match p e of
    [] ->
      case e of
        E _ -> e
        L es -> L $ fmap (rewriteAllWith (Rule p r)) es
    bs -> bind (head bs) r
